package caliban

import caliban.CalibanError.ValidationError
import zio.{ FiberRef, UIO, Unsafe, ZIO }

sealed trait HttpRequestMethod

object HttpRequestMethod {
  case object GET  extends HttpRequestMethod
  case object POST extends HttpRequestMethod

  private[caliban] val MutationOverGetError: ValidationError =
    ValidationError("Mutations are not allowed for GET requests", "")

  private val fiberRef: FiberRef[HttpRequestMethod] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(POST))

  private[caliban] val get: UIO[HttpRequestMethod] = fiberRef.get

  private[caliban] def setWith[R, E, B](method: HttpRequestMethod)(zio: ZIO[R, E, B]): ZIO[R, E, B] =
    method match {
      case POST => zio
      case GET  => fiberRef.locally(method)(zio)
    }
}
