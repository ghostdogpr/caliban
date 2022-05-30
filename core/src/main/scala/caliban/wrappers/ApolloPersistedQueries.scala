package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.Value.{ NullValue, StringValue }
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper.{ EffectfulWrapper, OverallWrapper, ParsingWrapper }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue }
import zio.{ Has, Layer, Ref, UIO, ZIO }

import java.nio.charset.StandardCharsets
import scala.collection.mutable

object ApolloPersistedQueries {

  type ApolloPersistence = Has[Service]

  trait Service {
    def get(hash: String): UIO[Option[Document]]
    def add(hash: String, query: Document): UIO[Unit]
  }

  object Service {
    val live: UIO[Service] = Ref.make[Map[String, Document]](Map()).map { cache =>
      new Service {
        override def get(hash: String): UIO[Option[Document]]      = cache.get.map(_.get(hash))
        override def add(hash: String, query: Document): UIO[Unit] = cache.update(_.updated(hash, query))
      }
    }
  }

  val live: Layer[Nothing, ApolloPersistence] = Service.live.toLayer

  private def parsingWrapper(docVar: Ref[Option[Either[String, Document]]]): ParsingWrapper[ApolloPersistence] =
    new ParsingWrapper[ApolloPersistence] {
      override def wrap[R1 <: ApolloPersistence](
        f: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          docVar.getAndSet(None).flatMap {
            case Some(Right(doc)) => ZIO.succeed(doc)
            case Some(Left(hash)) => f(query).tap(doc => ZIO.serviceWith[Service](_.add(hash, doc)))
            case None             => f(query)
          }
    }

  /**
   * If the query is using the persisted query protocol then this wrapper will set the inner `Either` of the `Ref` to
   * be a `Right(document)` where the document is the persisted query document. If the query isn't yet cached this will set the
   * `Left(hash)` which will then get passed to the parsing wrapper where it will populate the cache with the validated query document
   */
  private def overrallWrapper(docVar: Ref[Option[Either[String, Document]]]): OverallWrapper[ApolloPersistence] =
    new OverallWrapper[ApolloPersistence] {
      def wrap[R1 <: ApolloPersistence](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          readHash(request) match {
            case Some(hash) =>
              ZIO
                .serviceWith[Service](_.get(hash))
                .flatMap {
                  case Some(doc) => docVar.set(Some(Right(doc))) as request
                  case None      =>
                    request.query match {
                      case Some(value) if checkHash(hash, value) => docVar.set(Some(Left(hash))) as request
                      case Some(_)                               => ZIO.fail(ValidationError("Provided sha does not match any query", ""))
                      case None                                  => ZIO.fail(ValidationError("PersistedQueryNotFound", ""))
                    }

                }
                .flatMap(process)
                .catchAll(ex => UIO(GraphQLResponse(NullValue, List(ex))))
            case None       => process(request)
          }
    }

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   */
  val apolloPersistedQueries: EffectfulWrapper[ApolloPersistence] =
    EffectfulWrapper(Ref.make[Option[Either[String, Document]]](None).map { docVar =>
      overrallWrapper(docVar) |+| parsingWrapper(docVar)
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
