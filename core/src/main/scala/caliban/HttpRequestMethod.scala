package caliban

import caliban.CalibanError.ValidationError
import zio.{ FiberRef, Scope, UIO, URIO, Unsafe, ZIO }

private[caliban] sealed trait HttpRequestMethod

private[caliban] object HttpRequestMethod {
  case object GET  extends HttpRequestMethod
  case object POST extends HttpRequestMethod

  val MutationOverGetError: ValidationError = ValidationError("Mutations are not allowed for GET requests", "")

  private val fiberRef: FiberRef[HttpRequestMethod] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(POST))

  val get: UIO[HttpRequestMethod] = fiberRef.get

  def setWith[R, E, B](method: HttpRequestMethod)(zio: ZIO[R, E, B]): ZIO[R, E, B] =
    method match {
      case POST => zio
      case GET  => fiberRef.locally(method)(zio)
    }
}
