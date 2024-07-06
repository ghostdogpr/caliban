package caliban

import caliban.CalibanError.ValidationError
import caliban.Value.StringValue
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.{ FiberRef, Trace, Unsafe, ZIO }

private[caliban] sealed trait HttpRequestMethod

private[caliban] object HttpRequestMethod {
  case object GET extends HttpRequestMethod {
    val asStringValue: StringValue = StringValue("GET")
  }

  case object POST extends HttpRequestMethod

  final val ExtensionKey = "requestHttpMethod"

  val MutationOverGetError: ValidationError = ValidationError("Mutations are not allowed for GET requests", "")

  def updateRequest(isGetRequest: Boolean)(req: GraphQLRequest): GraphQLRequest =
    if (isGetRequest)
      req.withExtension(HttpRequestMethod.ExtensionKey, HttpRequestMethod.GET.asStringValue)
    else req

  def isGetRequest(req: GraphQLRequest): Boolean =
    req.extensions.flatMap(_.get(ExtensionKey)).contains(GET.asStringValue)

  @deprecated("To be removed in the next major release")
  def getWith[R, E, A](zio: HttpRequestMethod => ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    zio(HttpRequestMethod.POST)

  @deprecated("To be removed in the next major release")
  def setWith[R, E, B](method: HttpRequestMethod)(zio: ZIO[R, E, B])(implicit trace: Trace): ZIO[R, E, B] =
    zio
}
