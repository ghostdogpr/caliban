package caliban

import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, OverflowStrategy }
import caliban.AkkaHttpAdapter.convertWebSocketEndpoint
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, ZioWebSockets }
import caliban.interop.tapir.{ HttpInterpreter, HttpUploadInterpreter, TapirAdapter, WebSocketInterpreter }
import sttp.capabilities.WebSockets
import sttp.capabilities.akka.AkkaStreams
import sttp.capabilities.akka.AkkaStreams.Pipe
import sttp.model.StatusCode
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.PublicEndpoint
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.akkahttp.{ AkkaHttpServerInterpreter, AkkaHttpServerOptions }
import zio._
import zio.stream.ZStream

import scala.concurrent.{ ExecutionContext, Future }

class AkkaHttpAdapter private (private val options: AkkaHttpServerOptions)(implicit ec: ExecutionContext) {
  private val akkaInterpreter = AkkaHttpServerInterpreter(options)(ec)

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit runtime: Runtime[R]): Route =
    akkaInterpreter.toRoute(interpreter.serverEndpoints[R].map(TapirAdapter.convertHttpEndpointToFuture(_)))

  def makeHttpUploadService[R, E](interpreter: HttpUploadInterpreter[R, E])(implicit
    runtime: Runtime[R],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): Route =
    akkaInterpreter.toRoute(TapirAdapter.convertHttpEndpointToFuture(interpreter.serverEndpoint[R]))

  def makeWebSocketService[R, E](
    interpreter: WebSocketInterpreter[R, E]
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): Route =
    akkaInterpreter.toRoute(
      convertWebSocketEndpoint(
        interpreter
          .serverEndpoint[R]
          .asInstanceOf[
            ServerEndpoint.Full[
              Unit,
              Unit,
              (ServerRequest, String),
              StatusCode,
              (String, CalibanPipe),
              ZioWebSockets,
              RIO[R, *]
            ]
          ]
      )
    )
}

object AkkaHttpAdapter {

  def default(implicit ec: ExecutionContext): AkkaHttpAdapter =
    apply(AkkaHttpServerOptions.default)

  def apply(options: AkkaHttpServerOptions)(implicit ec: ExecutionContext): AkkaHttpAdapter =
    new AkkaHttpAdapter(options)

  type AkkaPipe = Flow[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput], Any]

  def convertWebSocketEndpoint[R](
    endpoint: ServerEndpoint.Full[
      Unit,
      Unit,
      (ServerRequest, String),
      StatusCode,
      (String, CalibanPipe),
      ZioWebSockets,
      RIO[
        R,
        *
      ]
    ]
  )(implicit
    ec: ExecutionContext,
    runtime: Runtime[R],
    materializer: Materializer
  ): ServerEndpoint[AkkaStreams with WebSockets, Future] =
    ServerEndpoint[
      Unit,
      Unit,
      (ServerRequest, String),
      StatusCode,
      (String, AkkaPipe),
      AkkaStreams with WebSockets,
      Future
    ](
      endpoint.endpoint
        .asInstanceOf[
          PublicEndpoint[
            (ServerRequest, String),
            StatusCode,
            (String, Pipe[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]),
            Any
          ]
        ],
      _ => _ => Future.successful(Right(())),
      _ =>
        _ =>
          req =>
            Unsafe
              .unsafe(implicit u => runtime.unsafe.runToFuture(endpoint.logic(zioMonadError)(())(req)).future)
              .map(_.map { case (protocol, zioPipe) =>
                val io =
                  for {
                    inputQueue     <- Queue.unbounded[GraphQLWSInput]
                    input           = ZStream.fromQueue(inputQueue)
                    output          = zioPipe(input)
                    sink            = Sink.foreachAsync[GraphQLWSInput](1)(input =>
                                        Unsafe
                                          .unsafe(implicit u => runtime.unsafe.runToFuture(inputQueue.offer(input).unit).future)
                                      )
                    (queue, source) =
                      Source.queue[Either[GraphQLWSClose, GraphQLWSOutput]](0, OverflowStrategy.fail).preMaterialize()
                    fiber          <- output.foreach(msg => ZIO.fromFuture(_ => queue.offer(msg))).forkDaemon
                    flow            = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() { (_, f) =>
                                        f.onComplete(_ =>
                                          Unsafe
                                            .unsafe(implicit u => runtime.unsafe.run(fiber.interrupt).getOrThrowFiberFailure())
                                        )
                                      }
                  } yield (protocol, flow)
                Unsafe
                  .unsafe(implicit u => runtime.unsafe.run(io).getOrThrowFiberFailure())
              })
    )
}
