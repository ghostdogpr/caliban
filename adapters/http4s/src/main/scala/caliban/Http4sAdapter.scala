package caliban

import caliban.execution.QueryExecution
import caliban.interop.cats.CatsInterop
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, ZioWebSockets }
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import cats.data.Kleisli
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.~>
import org.http4s._
import org.http4s.server.websocket.WebSocketBuilder2
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.Endpoint
import sttp.tapir.json.circe._
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.duration.Duration
import zio.interop.catz.concurrentInstance
import zio.random.Random

object Http4sAdapter {

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  ): HttpRoutes[RIO[R with Clock with Blocking, *]] = {
    val endpoints = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    ZHttp4sServerInterpreter().from(endpoints).toRoutes
  }

  def makeHttpServiceF[F[_]: Async, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit runtime: Runtime[R]): HttpRoutes[F] = {
    val endpoints  = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    val endpointsF = endpoints.map(convertHttpEndpointToF[F, R, E])
    Http4sServerInterpreter().toRoutes(endpointsF)
  }

  def makeHttpUploadService[R <: Has[_] with Random, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  ): HttpRoutes[RIO[R with Clock with Blocking, *]] = {
    val endpoint = TapirAdapter.makeHttpUploadService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    ZHttp4sServerInterpreter().from(endpoint).toRoutes
  }

  def makeHttpUploadServiceF[F[_]: Async, R <: Has[_] with Random, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit runtime: Runtime[R]): HttpRoutes[F] = {
    val endpoint  = TapirAdapter.makeHttpUploadService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    val endpointF = convertHttpEndpointToF[F, R, E](endpoint)
    Http4sServerInterpreter().toRoutes(endpointF)
  }

  def makeWebSocketService[R, R1 <: R, E](
    builder: WebSocketBuilder2[RIO[R with Clock with Blocking, *]],
    interpreter: GraphQLInterpreter[R1, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty,
    webSocketHooks: WebSocketHooks[R1, E] = WebSocketHooks.empty
  ): HttpRoutes[RIO[R1 with Clock with Blocking, *]] = {
    val endpoint = TapirAdapter.makeWebSocketService[R1, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      keepAliveTime,
      queryExecution,
      requestInterceptor,
      webSocketHooks
    )
    ZHttp4sServerInterpreter[R1]()
      .fromWebSocket(endpoint)
      .toRoutes(builder.asInstanceOf[WebSocketBuilder2[RIO[R1 with Clock with Blocking, *]]])
  }

  def makeWebSocketServiceF[F[_]: Async: Dispatcher, R, E](
    builder: WebSocketBuilder2[F],
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit runtime: Runtime[R]): HttpRoutes[F] = {
    val endpoint  = TapirAdapter.makeWebSocketService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      keepAliveTime,
      queryExecution,
      requestInterceptor,
      webSocketHooks
    )
    val endpointF = convertWebSocketEndpointToF[F, R, E](endpoint)
    Http4sServerInterpreter().toWebSocketRoutes(endpointF)(builder)
  }

  /**
   * Utility function to create an http4s middleware that can extracts something from each request
   * and provide a layer to eliminate the ZIO environment
   * @param route an http4s route
   * @param f a function from a request to a ZLayer
   * @tparam R the environment type to eliminate
   * @return a new route without the R requirement
   */
  def provideLayerFromRequest[R <: Has[_]](route: HttpRoutes[RIO[R, *]], f: Request[Task] => TaskLayer[R])(implicit
    tagged: Tag[R]
  ): HttpRoutes[Task] =
    Kleisli { (req: Request[Task[*]]) =>
      val to: Task ~> RIO[R, *] = new (Task ~> RIO[R, *]) {
        def apply[A](fa: Task[A]): RIO[R, A] = fa
      }

      val from: RIO[R, *] ~> Task = new (RIO[R, *] ~> Task) {
        def apply[A](fa: RIO[R, A]): Task[A] = fa.provideLayer(f(req))
      }

      route(req.mapK(to)).mapK(from).map(_.mapK(from))
    }

  /**
   * Utility function to create an http4s middleware that can extracts something from each request
   * and provide a layer to eliminate some part of the ZIO environment
   * @param route an http4s route
   * @param f a function from a request to a ZLayer
   * @tparam R the remaining environment
   * @tparam R1 the environment to eliminate
   * @return a new route that requires only R
   */
  def provideSomeLayerFromRequest[R <: Has[_], R1 <: Has[_]](
    route: HttpRoutes[RIO[R with R1, *]],
    f: Request[RIO[R, *]] => RLayer[R, R1]
  )(implicit tagged: Tag[R1]): HttpRoutes[RIO[R, *]] =
    Kleisli { (req: Request[RIO[R, *]]) =>
      val to: RIO[R, *] ~> RIO[R with R1, *] = new (RIO[R, *] ~> RIO[R with R1, *]) {
        def apply[A](fa: RIO[R, A]): RIO[R with R1, A] = fa
      }

      val from: RIO[R with R1, *] ~> RIO[R, *] = new (RIO[R with R1, *] ~> RIO[R, *]) {
        def apply[A](fa: RIO[R with R1, A]): RIO[R, A] = fa.provideSomeLayer[R](f(req))
      }

      route(req.mapK(to)).mapK(from).map(_.mapK(from))
    }

  /**
   * If you wish to use `Http4sServerInterpreter` with cats-effect IO instead of `ZHttp4sServerInterpreter`,
   * you can use this function to convert the tapir endpoints to their cats-effect counterpart.
   */
  def convertHttpEndpointToF[F[_]: Async, R, E](
    endpoint: ServerEndpoint[Any, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[Any, F] =
    ServerEndpoint[endpoint.A, endpoint.U, endpoint.I, endpoint.E, endpoint.O, Any, F](
      endpoint.endpoint,
      _ => a => CatsInterop.toEffect(endpoint.securityLogic(zioMonadError)(a)),
      _ => u => req => CatsInterop.toEffect(endpoint.logic(zioMonadError)(u)(req))
    )

  /**
   * If you wish to use `Http4sServerInterpreter` with cats-effect IO instead of `ZHttp4sServerInterpreter`,
   * you can use this function to convert the tapir endpoints to their cats-effect counterpart.
   */
  def convertWebSocketEndpointToF[F[_]: Async: Dispatcher, R, E](
    endpoint: ServerEndpoint[ZioWebSockets, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[Fs2Streams[F] with WebSockets, F] = {
    type Fs2Pipe = fs2.Pipe[F, GraphQLWSInput, GraphQLWSOutput]

    val e = endpoint
      .asInstanceOf[
        ServerEndpoint.Full[endpoint.A, endpoint.U, endpoint.I, endpoint.E, CalibanPipe, ZioWebSockets, RIO[R, *]]
      ]

    ServerEndpoint[endpoint.A, endpoint.U, endpoint.I, endpoint.E, Fs2Pipe, Fs2Streams[F] with WebSockets, F](
      e.endpoint.asInstanceOf[Endpoint[endpoint.A, endpoint.I, endpoint.E, Fs2Pipe, Any]],
      _ => a => CatsInterop.toEffect(e.securityLogic(zioMonadError)(a)),
      _ =>
        u =>
          req =>
            CatsInterop.toEffect(
              e.logic(zioMonadError)(u)(req)
                .map(_.map { zioPipe =>
                  import zio.stream.interop.fs2z._
                  fs2InputStream =>
                    zioPipe(fs2InputStream.translate(CatsInterop.fromEffectK[F, Any]).toZStream()).toFs2Stream
                      .translate(CatsInterop.toEffectK)
                })
            )
    )
  }

}
