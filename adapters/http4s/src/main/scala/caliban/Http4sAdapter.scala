package caliban

import caliban.interop.cats.{ CatsInterop, ToEffect }
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, ZioWebSockets }
import caliban.interop.tapir.{ HttpInterpreter, HttpUploadInterpreter, StreamConstructor, WebSocketInterpreter }
import cats.data.Kleisli
import cats.effect.Async
import cats.~>
import org.http4s._
import org.http4s.server.websocket.WebSocketBuilder2
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.Endpoint
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio._
import zio.interop.catz.concurrentInstance
import zio.stream.interop.fs2z._
import zio.stream.ZStream

object Http4sAdapter {

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E]): HttpRoutes[RIO[R, *]] =
    ZHttp4sServerInterpreter()
      .from(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))
      .toRoutes

  def makeHttpServiceF[F[_]: Async, R, E](
    interpreter: HttpInterpreter[R, E]
  )(implicit interop: ToEffect[F, R]): HttpRoutes[F] = {
    val endpoints  = interpreter.serverEndpoints[R, Fs2Streams[F]](Fs2Streams[F])
    val endpointsF = endpoints.map(convertHttpEndpointToF[F, R])
    Http4sServerInterpreter().toRoutes(endpointsF)
  }

  def makeHttpUploadService[R, E](interpreter: HttpUploadInterpreter[R, E])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): HttpRoutes[RIO[R, *]] =
    ZHttp4sServerInterpreter().from(interpreter.serverEndpoint[R, ZioStreams](ZioStreams)).toRoutes

  def makeHttpUploadServiceF[F[_]: Async, R, E](interpreter: HttpUploadInterpreter[R, E])(implicit
    interop: ToEffect[F, R],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): HttpRoutes[F] = {
    val endpoint  = interpreter.serverEndpoint[R, Fs2Streams[F]](Fs2Streams[F])
    val endpointF = convertHttpEndpointToF[F, R](endpoint)
    Http4sServerInterpreter().toRoutes(endpointF)
  }

  def makeWebSocketService[R, R1 <: R, E](
    builder: WebSocketBuilder2[RIO[R, *]],
    interpreter: WebSocketInterpreter[R1, E]
  ): HttpRoutes[RIO[R1, *]] =
    ZHttp4sServerInterpreter[R1]()
      .fromWebSocket(interpreter.serverEndpoint[R1])
      .toRoutes(builder.asInstanceOf[WebSocketBuilder2[RIO[R1, *]]])

  def makeWebSocketServiceF[F[_]: Async, R, E](builder: WebSocketBuilder2[F], interpreter: WebSocketInterpreter[R, E])(
    implicit
    interop: CatsInterop[F, R],
    runtime: Runtime[R]
  ): HttpRoutes[F] = {
    val endpointF = convertWebSocketEndpointToF[F, R](interpreter.serverEndpoint[R])
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
  def provideLayerFromRequest[R](route: HttpRoutes[RIO[R, *]], f: Request[Task] => TaskLayer[R]): HttpRoutes[Task] =
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
    endpoint: ServerEndpoint[Fs2Streams[F], RIO[R, *]]
  )(implicit interop: ToEffect[F, R]): ServerEndpoint[Fs2Streams[F], F] =
    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      endpoint.OUTPUT,
      Fs2Streams[F],
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

  private implicit def streamConstructor[F[_], R](implicit
    toEffect: ToEffect[F, R]
  ): StreamConstructor[Fs2Streams[F]#BinaryStream] =
    new StreamConstructor[Fs2Streams[F]#BinaryStream] {
      override def apply(stream: ZStream[Any, Throwable, Byte]): Fs2Streams[F]#BinaryStream =
        stream.toFs2Stream
          .translate(toEffect.toEffectK)
    }

}
