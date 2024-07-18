package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.Value._
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper._
import caliban._
import zio._

import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
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

    val live: UIO[ApolloPersistence] = ZIO.succeed(new Live)(Trace.empty)

    final class Live extends ApolloPersistence {
      private implicit val trace: Trace                         = Trace.empty
      private val docCache: ConcurrentHashMap[String, Document] = new ConcurrentHashMap[String, Document]()

      override def get(hash: String): UIO[Option[Document]]      = ZIO.succeed(Option(docCache.get(hash)))
      override def add(hash: String, query: Document): UIO[Unit] = ZIO.succeed(docCache.put(hash, query)).unit
    }

  }

  val live: Layer[Nothing, ApolloPersistence] = ZLayer(ApolloPersistence.live)

  private def parsingWrapper(
    docVar: AtomicReference[Option[(String, Option[Document])]]
  ): ParsingWrapper[Any] =
    new ParsingWrapper[Any] {
      override def wrap[R1 <: Any](
        f: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          docVar.get() match {
            case Some((_, Some(doc))) => Exit.succeed(doc)
            case _                    => f(query)
          }
    }

  private def validationWrapper(
    store: ApolloPersistence,
    docVar: AtomicReference[Option[(String, Option[Document])]]
  ): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      override val priority: Int = 100

      override def wrap[R1 <: Any](
        f: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          docVar.get() match {
            case Some((_, Some(_))) => Configurator.ref.locallyWith(_.copy(skipValidation = true))(f(doc))
            case Some((hash, None)) => f(doc) <* store.add(hash, doc)
            case None               => f(doc)
          }
    }

  /**
   * If the query is using the persisted query protocol then this wrapper will set the inner `Option[Document]` of the `Promise` to
   * be a `(_, Some(Document))` where the document is the persisted query document. If the query isn't yet cached this will set the
   * value to `(_, None)` which will then get passed to the parsing wrapper where it will populate the cache with the validated query document
   */
  private def overallWrapper(
    store: ApolloPersistence,
    docVar: AtomicReference[Option[(String, Option[Document])]]
  ): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1 <: Any](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          readHash(request) match {
            case Some(hash) =>
              store
                .get(hash)
                .flatMap {
                  case Some(doc) =>
                    docVar.set(Some((hash, Some(doc))))
                    Exit.succeed(request)
                  case None      =>
                    request.query match {
                      case Some(value) if checkHash(hash, value) =>
                        docVar.set(Some((hash, None)))
                        Exit.succeed(request)
                      case Some(_)                               => Exit.fail(ValidationError("Provided sha does not match any query", ""))
                      case None                                  => Exit.fail(ValidationError("PersistedQueryNotFound", ""))
                    }
                }
                .flatMap(process)
                .catchAll(ex => Exit.succeed(GraphQLResponse(NullValue, List(ex))))
            case None       => process(request)
          }
    }

  @deprecated("Use `wrapper` instead and pass the cache explicitly", "2.9.0")
  val apolloPersistedQueries: EffectfulWrapper[ApolloPersistence] =
    EffectfulWrapper(ZIO.serviceWith[ApolloPersistence](wrapper))

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   *
   * This wrapper will initialize a non-expiring cache which will be used for all queries
   *
   * @see Overloaded method for a variant that allows using a custom cache
   */
  def wrapper: Wrapper[Any] = wrapper(new ApolloPersistence.Live)

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   *
   * @param cache the query cache that will be used to store the parsed documents
   */
  def wrapper(cache: ApolloPersistence): Wrapper[Any] = Wrapper.suspend {
    val ref = new AtomicReference[Option[(String, Option[Document])]](None)
    overallWrapper(cache, ref) |+| parsingWrapper(ref) |+| validationWrapper(cache, ref)
  }

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

  private val HexArray: Array[Char] = "0123456789abcdef".toCharArray

  private def checkHash(hash: String, query: String): Boolean = {
    val sha256 = java.security.MessageDigest.getInstance("SHA-256")
    val digest = sha256.digest(query.getBytes(StandardCharsets.UTF_8))
    digestMatchesHash(digest, hash)
  }

  private def digestMatchesHash(digest: Array[Byte], hash: String): Boolean = {
    val chars = hash.toCharArray
    val size  = digest.length
    var j     = 0
    var res   = chars.length == size * 2
    while (j < size && res) {
      val v = digest(j) & 0xff
      if (chars(j * 2) == HexArray(v >>> 4) && chars(j * 2 + 1) == HexArray(v & 0xf)) ()
      else res = false
      j += 1
    }
    res
  }

}
