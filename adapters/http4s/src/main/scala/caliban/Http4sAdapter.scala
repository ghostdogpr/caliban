package caliban

import caliban.execution.QueryExecution
import caliban.interop.cats.{ CatsInterop, ToEffect }
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, TapirResponse, ZioWebSockets }
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import cats.data.Kleisli
import cats.effect.Async
import cats.~>
import org.http4s._
import org.http4s.server.websocket.WebSocketBuilder2
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.Endpoint
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio._
import zio.interop.catz.concurrentInstance

object Http4sAdapter {

  def makeHttpService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): HttpRoutes[RIO[R1, *]] =
    ???
//    val endpoints = TapirAdapter.makeHttpService[R1, R, E](interpreter, requestInterceptor)
//    ZHttp4sServerInterpreter().from(endpoints).toRoutes

  def makeHttpServiceF[F[_]: Async, R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    interop: ToEffect[F, R1],
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): HttpRoutes[F] =
    ???
//    val endpoints  = TapirAdapter.makeHttpService[R1, R, E](interpreter, requestInterceptor)
//    val endpointsF = endpoints.map(convertHttpEndpointToF[F, R1])
//    Http4sServerInterpreter().toRoutes(endpointsF)

  def makeHttpUploadService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): HttpRoutes[RIO[R1, *]] =
    ???
//    val endpoint = TapirAdapter.makeHttpUploadService[R1, R, E](interpreter, requestInterceptor)
//    ZHttp4sServerInterpreter().from(endpoint).toRoutes

  def makeHttpUploadServiceF[F[_]: Async, R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    interop: ToEffect[F, R1],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): HttpRoutes[F] =
    ???
//    val endpoint  = TapirAdapter.makeHttpUploadService[R1, R, E](interpreter, requestInterceptor)
//    val endpointF = convertHttpEndpointToF[F, R1](endpoint)
//    Http4sServerInterpreter().toRoutes(endpointF)

  def makeWebSocketService[R2, R, R1 <: R, E](
    builder: WebSocketBuilder2[RIO[R, *]],
    interpreter: GraphQLInterpreter[R1, E],
    keepAliveTime: Option[Duration] = None,
    requestInterceptor: ZLayer[R2 & ServerRequest, TapirResponse, R1] = ZLayer.empty,
    webSocketHooks: WebSocketHooks[R1, E] = WebSocketHooks.empty
  )(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): HttpRoutes[RIO[R2, *]] =
    ???
//    val endpoint =
//      TapirAdapter.makeWebSocketService[R2, R1, E](interpreter, keepAliveTime, requestInterceptor, webSocketHooks)
//    ZHttp4sServerInterpreter[R2]()
//      .fromWebSocket(endpoint)
//      .toRoutes(builder.asInstanceOf[WebSocketBuilder2[RIO[R2, *]]])

  def makeWebSocketServiceF[F[_]: Async, R1, R, E](
    builder: WebSocketBuilder2[F],
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration] = None,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit
    interop: CatsInterop[F, R1],
    runtime: Runtime[R1],
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): HttpRoutes[F] =
    ???
//    val endpoint  = TapirAdapter.makeWebSocketService[R1, R, E](
//      interpreter,
//      keepAliveTime,
//      requestInterceptor,
//      webSocketHooks
//    )
//    val endpointF = convertWebSocketEndpointToF[F, R1](endpoint)
//    Http4sServerInterpreter().toWebSocketRoutes(endpointF)(builder)

  /**
   * Utility function to create an http4s middleware that can extracts something from each request
   * and provide a layer to eliminate the ZIO environment
   * @param route an http4s route
   * @param f a function from a request to a ZLayer
   * @tparam R the environment type to eliminate
   * @return a new route without the R requirement
   */
  def provideLayerFromRequest[R](route: HttpRoutes[RIO[R, *]], f: Request[Task] => TaskLayer[R])(implicit
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
  def provideSomeLayerFromRequest[R, R1](
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
  def convertHttpEndpointToF[F[_], R](
    endpoint: ServerEndpoint[Any, RIO[R, *]]
  )(implicit interop: ToEffect[F, R]): ServerEndpoint[Any, F] =
    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      endpoint.OUTPUT,
      Any,
      F
    ](
      endpoint.endpoint,
      _ => a => interop.toEffect(endpoint.securityLogic(zioMonadError)(a)),
      _ => u => req => interop.toEffect(endpoint.logic(zioMonadError)(u)(req))
    )

  /**
   * If you wish to use `Http4sServerInterpreter` with cats-effect IO instead of `ZHttp4sServerInterpreter`,
   * you can use this function to convert the tapir endpoints to their cats-effect counterpart.
   */
  def convertWebSocketEndpointToF[F[_], R](
    endpoint: ServerEndpoint[ZioWebSockets, RIO[R, *]]
  )(implicit interop: CatsInterop[F, R], runtime: Runtime[R]): ServerEndpoint[Fs2Streams[F] with WebSockets, F] = {
    type Fs2Pipe = fs2.Pipe[F, GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]

    val e = endpoint
      .asInstanceOf[
        ServerEndpoint.Full[
          endpoint.SECURITY_INPUT,
          endpoint.PRINCIPAL,
          endpoint.INPUT,
          endpoint.ERROR_OUTPUT,
          (String, CalibanPipe),
          ZioWebSockets,
          RIO[R, *]
        ]
      ]

    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      (String, Fs2Pipe),
      Fs2Streams[F] with WebSockets,
      F
    ](
      e.endpoint
        .asInstanceOf[Endpoint[endpoint.SECURITY_INPUT, endpoint.INPUT, endpoint.ERROR_OUTPUT, (String, Fs2Pipe), Any]],
      _ => a => interop.toEffect(e.securityLogic(zioMonadError)(a)),
      _ =>
        u =>
          req =>
            interop.toEffect(
              e.logic(zioMonadError)(u)(req)
                .map(_.map { case (protocol, zioPipe) =>
                  import zio.stream.interop.fs2z._
                  (
                    protocol,
                    fs2InputStream =>
                      zioPipe(
                        fs2InputStream
                          .translate(interop.fromEffectK)
                          .toZStream()
                          .provideEnvironment(runtime.environment)
                      ).toFs2Stream
                        .translate(interop.toEffectK)
                  )
                })
            )
    )
  }

}
