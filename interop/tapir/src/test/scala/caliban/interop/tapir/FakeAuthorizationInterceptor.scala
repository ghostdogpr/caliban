package caliban.interop.tapir
import caliban.interop.tapir.TapirAdapter.TapirResponse
import sttp.model.StatusCode
import sttp.tapir.model.ServerRequest
import zio.ZIO

class FakeAuthorizationInterceptor[R](authenticate: ServerRequest => ZIO[R, (Int, String), Unit])
    extends RequestInterceptor[R, Any] {
  override def apply[R1 <: R, A](request: ServerRequest)(
    e: ZIO[R1 with Any, TapirAdapter.TapirResponse, A]
  ): ZIO[R1, TapirAdapter.TapirResponse, A] =
    authenticate(request).mapError { case (status, str) =>
      TapirResponse(StatusCode(status), body = str)
    } *> e

}

object FakeAuthorizationInterceptor {

  val bearer: RequestInterceptor[Any, Any] =
    new FakeAuthorizationInterceptor[Any](req =>
      ZIO.fail((401, "You are unauthorized!")).when(req.headers("X-Invalid").nonEmpty).unit
    )

}
