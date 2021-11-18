package caliban.interop.tapir

import sttp.model.StatusCode
import sttp.tapir.model.ServerRequest
import zio.ZIO

/**
 * RequestInterceptor provides a way to extract context from the http request, potentially failing before
 * query execution or injecting context into ZIO environment.
 */
trait RequestInterceptor[-R] { self =>
  def apply[R1 <: R](request: ServerRequest): ZIO[R1, StatusCode, Unit]

  def |+|[R1 <: R](that: RequestInterceptor[R1]): RequestInterceptor[R1] = new RequestInterceptor[R1] {
    override def apply[R2 <: R1](request: ServerRequest): ZIO[R2, StatusCode, Unit] =
      that.apply[R2](request) *> self.apply[R2](request)
  }
}

object RequestInterceptor {
  def empty: RequestInterceptor[Any] = new RequestInterceptor[Any] {
    override def apply[R](request: ServerRequest): ZIO[R, StatusCode, Unit] = ZIO.unit
  }
}
