package caliban

import caliban.CalibanError.ValidationError
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.{ FiberRef, Trace, Unsafe, ZIO }

private[caliban] sealed trait HttpRequestMethod

private[caliban] object HttpRequestMethod {
  case object GET  extends HttpRequestMethod
  case object POST extends HttpRequestMethod

  val MutationOverGetError: ValidationError = ValidationError("Mutations are not allowed for GET requests", "")

  private val fiberRef: FiberRef[HttpRequestMethod] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(POST))

  def getWith[R, E, A](zio: HttpRequestMethod => ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    fiberRef.getWith(zio)

  def setWith[R, E, B](method: HttpRequestMethod)(zio: ZIO[R, E, B])(implicit trace: Trace): ZIO[R, E, B] =
    method match {
      case POST => zio
      case GET  => fiberRef.locally(method)(zio)
    }
}
