package caliban

import caliban.CalibanError.ValidationError
import zio.{ FiberRef, Unsafe }

private[caliban] sealed trait HttpRequestMethod

private[caliban] object HttpRequestMethod {
  case object GET  extends HttpRequestMethod
  case object POST extends HttpRequestMethod

  val fiberRef: FiberRef[HttpRequestMethod] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(POST))
  val MutationOverGetError: ValidationError = ValidationError("Mutations are not allowed for GET requests", "")
}
