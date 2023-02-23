package caliban.interop.tapir

import caliban.interop.tapir.TapirAdapter.TapirResponse
import sttp.tapir.model.ServerRequest
import zio.ZIO

/**
 * RequestInterceptor provides a way to extract context from the http request, potentially failing before
 * query execution or injecting context into ZIO environment.
 */
trait RequestInterceptor[-R, +DynR] { self =>
  def apply[R1 <: R, A](request: ServerRequest)(e: ZIO[R1 with DynR, TapirResponse, A]): ZIO[R1, TapirResponse, A]

  def |+|[R1 <: R, DynR2](that: RequestInterceptor[R1, DynR2]): RequestInterceptor[R1, DynR with DynR2] =
    new RequestInterceptor[R1, DynR with DynR2] {
      override def apply[R2 <: R1, A](request: ServerRequest)(
        e: ZIO[R2 with DynR with DynR2, TapirResponse, A]
      ): ZIO[R2, TapirResponse, A] =
        that.apply[R2, A](request)(self.apply[R2 with DynR2, A](request)(e))
    }
}

object RequestInterceptor {
  val empty: RequestInterceptor[Any, Any] = new RequestInterceptor[Any, Any] {
    override def apply[R, A](request: ServerRequest)(e: ZIO[R with Any, TapirResponse, A]): ZIO[R, TapirResponse, A] = e
  }
}
