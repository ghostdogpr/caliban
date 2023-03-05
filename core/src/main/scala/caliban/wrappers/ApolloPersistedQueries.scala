package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.Value._
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.validation.Validator
import caliban.wrappers.Wrapper._
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue }
import zio._

import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

object ApolloPersistedQueries {

  trait ApolloPersistence {
    def get(hash: String): UIO[Option[Document]]
    def add(hash: String, query: Document): UIO[Unit]
  }

  object ApolloPersistence {

    def get(hash: String): ZIO[ApolloPersistence, Nothing, Option[Document]]      =
      ZIO.serviceWithZIO[ApolloPersistence](_.get(hash))
    def add(hash: String, query: Document): ZIO[ApolloPersistence, Nothing, Unit] =
      ZIO.serviceWithZIO[ApolloPersistence](_.add(hash, query))

    val live: UIO[ApolloPersistence] =
      ZIO.succeed(new ConcurrentHashMap[String, Document]()).map { docCache =>
        new ApolloPersistence {
          override def get(hash: String): UIO[Option[Document]]      = ZIO.succeed(Option(docCache.get(hash)))
          override def add(hash: String, query: Document): UIO[Unit] = ZIO.succeed(docCache.put(hash, query)).unit
        }
      }
  }

  val live: Layer[Nothing, ApolloPersistence] = ZLayer(ApolloPersistence.live)

  private def parsingWrapper(
    docVar: Promise[Nothing, Option[(String, Option[Document])]]
  ): ParsingWrapper[ApolloPersistence] =
    new ParsingWrapper[ApolloPersistence] {
      override def wrap[R1 <: ApolloPersistence](
        f: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          docVar.await.flatMap {
            case Some((_, Some(doc))) => ZIO.succeed(doc)
            case _                    => f(query)
          }
    }

  private def validationWrapper(
    docVar: Promise[Nothing, Option[(String, Option[Document])]]
  ): ValidationWrapper[ApolloPersistence] =
    new ValidationWrapper[ApolloPersistence] {
      override val priority: Int = 100

      override def wrap[R1 <: ApolloPersistence](
        f: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          docVar.await.flatMap {
            case Some((_, Some(_))) => Validator.skipQueryValidationRef.set(true) *> f(doc)
            case Some((hash, None)) => f(doc) <* ApolloPersistence.add(hash, doc)
            case None               => f(doc)
          }
    }

  /**
   * If the query is using the persisted query protocol then this wrapper will set the inner `Option[Document]` of the `Promise` to
   * be a `(_, Some(Document))` where the document is the persisted query document. If the query isn't yet cached this will set the
   * value to `(_, None)` which will then get passed to the parsing wrapper where it will populate the cache with the validated query document
   */
  private def overrallWrapper(
    docVar: Promise[Nothing, Option[(String, Option[Document])]]
  ): OverallWrapper[ApolloPersistence] =
    new OverallWrapper[ApolloPersistence] {
      def wrap[R1 <: ApolloPersistence](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          readHash(request) match {
            case Some(hash) =>
              ApolloPersistence
                .get(hash)
                .flatMap {
                  case Some(doc) => docVar.succeed(Some((hash, Some(doc)))) as request
                  case None      =>
                    request.query match {
                      case Some(value) if checkHash(hash, value) => docVar.succeed(Some((hash, None))).as(request)
                      case Some(_)                               => ZIO.fail(ValidationError("Provided sha does not match any query", ""))
                      case None                                  => ZIO.fail(ValidationError("PersistedQueryNotFound", ""))
                    }
                }
                .flatMap(process)
                .catchAll(ex => ZIO.succeed(GraphQLResponse(NullValue, List(ex))))
            case None       => docVar.succeed(None) *> process(request)
          }
    }

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   */
  val apolloPersistedQueries: EffectfulWrapper[ApolloPersistence] =
    EffectfulWrapper(Promise.make[Nothing, Option[(String, Option[Document])]].map { docVar =>
      overrallWrapper(docVar) |+| parsingWrapper(docVar) |+| validationWrapper(docVar)
    })

  private def readHash(request: GraphQLRequest): Option[String] =
    request.extensions
      .flatMap(_.get("persistedQuery"))
      .flatMap {
        case InputValue.ObjectValue(fields) =>
          fields.get("sha256Hash").collectFirst { case StringValue(hash) =>
            hash
          }
        case _                              => None
      }

  private def hex(bytes: Array[Byte]): String = {
    val builder = new mutable.StringBuilder(bytes.length * 2)
    bytes.foreach(byte => builder.append(f"$byte%02x".toLowerCase))
    builder.mkString
  }

  private def checkHash(hash: String, query: String): Boolean = {
    val sha256 = java.security.MessageDigest.getInstance("SHA-256")
    val digest = sha256.digest(query.getBytes(StandardCharsets.UTF_8))
    hex(digest) == hash
  }
}
