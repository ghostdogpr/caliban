package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.Value._
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper._
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue }
import zio._
import zio.concurrent.{ ConcurrentMap, ConcurrentSet }

import java.nio.charset.StandardCharsets
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object ApolloPersistedQueries {

  trait ApolloPersistence {
    def get(hash: String): UIO[Option[Document]]
    def add(hash: String, query: Document): UIO[Unit]
    def isValidated(hash: String, variablesHashCode: Int): UIO[Boolean]
    def registerValidation(hash: String, variablesHashCode: Int): UIO[Unit]
  }

  object ApolloPersistence {

    def get(hash: String): ZIO[ApolloPersistence, Nothing, Option[Document]]                            =
      ZIO.serviceWithZIO[ApolloPersistence](_.get(hash))
    def add(hash: String, query: Document): ZIO[ApolloPersistence, Nothing, Unit]                       =
      ZIO.serviceWithZIO[ApolloPersistence](_.add(hash, query))
    def isValidated(hash: String, variablesHashCode: Int): ZIO[ApolloPersistence, Nothing, Boolean]     =
      ZIO.serviceWithZIO[ApolloPersistence](_.isValidated(hash, variablesHashCode))
    def registerValidation(hash: String, variablesHashCode: Int): ZIO[ApolloPersistence, Nothing, Unit] =
      ZIO.serviceWithZIO[ApolloPersistence](_.registerValidation(hash, variablesHashCode))

    val live: UIO[ApolloPersistence] =
      (ConcurrentMap.empty[String, Document] <*> ConcurrentSet.empty[(String, Int)]).map {
        case (docCache, validationCache) =>
          new ApolloPersistence {
            override def get(hash: String): UIO[Option[Document]]                            = docCache.get(hash)
            override def add(hash: String, query: Document): UIO[Unit]                       = docCache.put(hash, query).unit
            override def isValidated(hash: String, variablesHashCode: Int): UIO[Boolean]     =
              validationCache.contains(hash -> variablesHashCode)
            override def registerValidation(hash: String, variablesHashCode: Int): UIO[Unit] =
              validationCache.add(hash -> variablesHashCode).unit
          }
      }
  }

  val live: Layer[Nothing, ApolloPersistence] = ZLayer(ApolloPersistence.live)

  private def parsingWrapper(
    docVar: Promise[Nothing, Option[(String, GraphQLRequest, Option[Document])]]
  ): ParsingWrapper[ApolloPersistence] =
    new ParsingWrapper[ApolloPersistence] {
      override def wrap[R1 <: ApolloPersistence](
        f: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          docVar.await.flatMap {
            case Some((_, _, Some(doc))) => ZIO.succeed(doc)
            case Some((hash, _, None))   => f(query).tap(doc => ApolloPersistence.add(hash, doc))
            case None                    => f(query)
          }
    }

  private def validationWrapper(
    docVar: Promise[Nothing, Option[(String, GraphQLRequest, Option[Document])]]
  ): ValidationWrapper[ApolloPersistence] =
    new ValidationWrapper[ApolloPersistence] {
      override def wrap[R1 <: ApolloPersistence](
        f: Wrapper.ValidationWrapperInput => ZIO[R1, ValidationError, ExecutionRequest]
      ): Wrapper.ValidationWrapperInput => ZIO[R1, ValidationError, ExecutionRequest] =
        (input: ValidationWrapperInput) =>
          docVar.await.flatMap {
            case Some((hash, req, _)) =>
              hashVariables(req.variables).flatMap { variablesHash =>
                ApolloPersistence.isValidated(hash, variablesHash).flatMap { isCached =>
                  if (isCached) f(input.copy(skipValidation = true))
                  else f(input) <* ApolloPersistence.registerValidation(hash, variablesHash)
                }
              }
            case None                 => f(input)
          }

      /**
       * In order to cache the validation step against different input variables, we need to strip the value while
       * retaining the type, with the exception of enums since the validation is performed against the enum value
       */
      private def hashVariables(variables: Option[Map[String, InputValue]]): UIO[Int] = {
        def stripVariableValues: InputValue => UIO[InputValue] = {
          case NullValue                      => ZIO.succeed(NullValue)
          case IntValue.IntNumber(_)          => ZIO.succeed(IntValue.IntNumber(0))
          case IntValue.LongNumber(_)         => ZIO.succeed(IntValue.LongNumber(0L))
          case IntValue.BigIntNumber(_)       => ZIO.succeed(IntValue.BigIntNumber(0))
          case FloatValue.FloatNumber(_)      => ZIO.succeed(FloatValue.FloatNumber(0f))
          case FloatValue.DoubleNumber(_)     => ZIO.succeed(FloatValue.DoubleNumber(0d))
          case FloatValue.BigDecimalNumber(_) => ZIO.succeed(FloatValue.BigDecimalNumber(0))
          case StringValue(_)                 => ZIO.succeed(StringValue("?"))
          case BooleanValue(_)                => ZIO.succeed(BooleanValue(true))
          case EnumValue(v)                   => ZIO.succeed(EnumValue(v))
          case InputValue.ListValue(l)        => ZIO.foreach(l)(stripVariableValues).map(InputValue.ListValue(_))
          case InputValue.ObjectValue(o)      =>
            val sortedMap = TreeMap.empty[String, InputValue] ++ o
            ZIO.foreach(sortedMap) { case (k, v) => stripVariableValues(v).map(k -> _) }.map(InputValue.ObjectValue(_))
          case InputValue.VariableValue(_)    => ZIO.succeed(InputValue.VariableValue("?"))
        }
        stripVariableValues(InputValue.ObjectValue(variables.getOrElse(Map.empty))).map(_.hashCode())
      }

    }

  /**
   * If the query is using the persisted query protocol then this wrapper will set the inner `Option[Document]` of the `Promise` to
   * be a `(_, _, Some(Document))` where the document is the persisted query document. If the query isn't yet cached this will set the
   * value to `(_, _, None)` which will then get passed to the parsing wrapper where it will populate the cache with the validated query document
   */
  private def overrallWrapper(
    docVar: Promise[Nothing, Option[(String, GraphQLRequest, Option[Document])]]
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
                  case Some(doc) => docVar.succeed(Some((hash, request, Some(doc)))) as request
                  case None      =>
                    request.query match {
                      case Some(value) if checkHash(hash, value) =>
                        docVar.succeed(Some((hash, request, None))).as(request)
                      case Some(_)                               => ZIO.fail(ValidationError("Provided sha does not match any query", ""))
                      case None                                  => ZIO.fail(ValidationError("PersistedQueryNotFound", ""))
                    }

                }
                .flatMap(process)
                .catchAll(ex => ZIO.succeed(GraphQLResponse(NullValue, List(ex))))
            case None       => process(request)
          }
    }

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   */
  val apolloPersistedQueries: EffectfulWrapper[ApolloPersistence] =
    EffectfulWrapper(Promise.make[Nothing, Option[(String, GraphQLRequest, Option[Document])]].map { docVar =>
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
