package caliban.interop.tapir

import caliban.interop.tapir.TapirAdapter.TapirResponse
import sttp.model.StatusCode
import sttp.tapir.model.ServerRequest
import zio.{ ZIO, ZLayer }

object FakeAuthorizationInterceptor {

  def make[R](
    authenticate: ServerRequest => ZIO[R, (Int, String), Unit]
  ): ZLayer[ServerRequest with R, TapirResponse, R] =
    ZLayer.fromZIOEnvironment {
      for {
        request <- ZIO.service[ServerRequest]
        env     <- ZIO.environment[R]
        _       <- authenticate(request).mapError { case (status, str) =>
                     TapirResponse(StatusCode(status), body = str)
                   }
      } yield env
    }

  def bearer[R]: ZLayer[ServerRequest with R, TapirResponse, R] =
    make[R](req => ZIO.fail((401, "You are unauthorized!")).when(req.headers("X-Invalid").nonEmpty).unit)
}
