package caliban.gateway

import caliban.GraphQLRequest
import zio.ZIO

import scala.util.control.NoStackTrace

/**
 * Supplies the GraphQL query text for an incoming request, for example by looking up a persisted document ID.
 * Resolution runs on every request, before operation-cache lookup, and replaces only the query text.
 * Used by `executeRequest` and `explain(request)`; `check(query)` validates literal text without this hook.
 */
final class OperationResolver[-R] private[gateway] (
  private[gateway] val resolve: GraphQLRequest => ZIO[R, Throwable, String],
  private[gateway] val cacheable: Boolean
)

object OperationResolver {

  /**
   * An intentional public rejection. Return with `ZIO.fail` to expose `message` and `extensions.code` to clients.
   * Defects, including a Rejection thrown while constructing the resolver effect, remain masked.
   */
  final case class Rejection(message: String, code: String) extends Exception(message) with NoStackTrace

  /**
   * Creates a resolver that allows parsed documents and execution plans to be cached.
   * `resolve` runs on every request, before parsing and validation, even when preparation is cached.
   * Its result replaces the request's query text, preserving operation name, variables, and extensions.
   * Fail with [[Rejection]] to return a public message and error code; unexpected failures are masked.
   */
  def apply[R](resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, cacheable = true)

  /**
   * Creates a resolver whose operations bypass prepared-document and plan caching.
   * Both this resolver and a cacheable resolver run on every request.
   */
  def uncached[R](resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, cacheable = false)

  /**
   * Resolves opaque IDs against an immutable registry of trusted documents, using normal operation caching.
   * The extractor should return None for missing or malformed IDs. None and empty IDs are rejected with
   * TRUSTED_DOCUMENT_ID_INVALID; unregistered IDs are rejected with TRUSTED_DOCUMENT_NOT_FOUND.
   * Registered text overrides any supplied query text. This helper never registers documents or falls back
   * to client text, and does not replace request-specific authorization through OperationPolicy.
   */
  def trustedDocuments(documents: Map[String, String])(
    extractId: GraphQLRequest => Option[String]
  ): OperationResolver[Any] =
    apply[Any] { request =>
      extractId(request).filter(_.nonEmpty) match {
        case None     =>
          ZIO.fail(Rejection("A non-empty trusted document ID is required.", "TRUSTED_DOCUMENT_ID_INVALID"))
        case Some(id) =>
          documents.get(id) match {
            case Some(document) => ZIO.succeed(document)
            case None           => ZIO.fail(Rejection("Trusted document not found.", "TRUSTED_DOCUMENT_NOT_FOUND"))
          }
      }
    }
}
