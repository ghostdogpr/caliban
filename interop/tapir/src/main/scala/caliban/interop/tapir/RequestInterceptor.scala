package caliban.interop.tapir

import sttp.model.StatusCode
import sttp.tapir.model.ServerRequest
import zio.ZIO

/**
 * RequestInterceptor provides a way to extract context from the http request, potentially failing before
 * query execution or injecting context into ZIO environment.
 */
trait RequestInterceptor[-R] { self =>
  def apply[R1 <: R, A](request: ServerRequest)(e: ZIO[R1, StatusCode, A]): ZIO[R1, StatusCode, A]

  def |+|[R1 <: R](that: RequestInterceptor[R1]): RequestInterceptor[R1] = new RequestInterceptor[R1] {
    override def apply[R2 <: R1, A](request: ServerRequest)(e: ZIO[R2, StatusCode, A]): ZIO[R2, StatusCode, A] =
      that.apply[R2, A](request)(self.apply[R2, A](request)(e))
  }
}

object RequestInterceptor {
  def empty: RequestInterceptor[Any] = new RequestInterceptor[Any] {
    override def apply[R, A](request: ServerRequest)(e: ZIO[R, StatusCode, A]): ZIO[R, StatusCode, A] = e
  }
}
