package caliban

import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, OverflowStrategy }
import akka.util.ByteString
import caliban.PlayAdapter.convertHttpStreamingEndpoint
import caliban.interop.tapir.TapirAdapter._
import caliban.interop.tapir.{ HttpInterpreter, HttpUploadInterpreter, WebSocketInterpreter }
import play.api.routing.Router.Routes
import sttp.capabilities.WebSockets
import sttp.capabilities.akka.AkkaStreams
import sttp.capabilities.akka.AkkaStreams.Pipe
import sttp.capabilities.zio.ZioStreams
import sttp.model.{ MediaType, StatusCode }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.PublicEndpoint
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.play.{ PlayServerInterpreter, PlayServerOptions }
import zio._
import zio.stream.ZStream

import scala.concurrent.Future

class PlayAdapter private (private val options: Option[PlayServerOptions]) {
  private def playInterpreter(implicit mat: Materializer) =
    options.fold(PlayServerInterpreter())(PlayServerInterpreter(_))

  def makeHttpService[R, E](
    interpreter: HttpInterpreter[R, E]
  )(implicit runtime: Runtime[R], materializer: Materializer): Routes =
    playInterpreter.toRoutes(
      interpreter.serverEndpoints[R].map(convertHttpStreamingEndpoint[R, (GraphQLRequest, ServerRequest)])
    )

  def makeHttpUploadService[R, E](interpreter: HttpUploadInterpreter[R, E])(implicit
    runtime: Runtime[R],
    materializer: Materializer,
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): Routes =
    playInterpreter.toRoutes(convertHttpStreamingEndpoint(interpreter.serverEndpoint[R]))

  def makeWebSocketService[R, E](
    interpreter: WebSocketInterpreter[R, E]
  )(implicit runtime: Runtime[R], materializer: Materializer): Routes = {
    val endpoint = interpreter.serverEndpoint[R]
    playInterpreter.toRoutes(
      PlayAdapter.convertWebSocketEndpoint(
        endpoint.asInstanceOf[
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
}

object PlayAdapter extends PlayAdapter(None) {

  def apply(options: PlayServerOptions) =
    new PlayAdapter(Some(options))

  type AkkaPipe = Flow[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput], Any]

  def convertHttpStreamingEndpoint[R, Input](
    endpoint: ServerEndpoint.Full[
      Unit,
      Unit,
      Input,
      TapirResponse,
      CalibanResponse,
      ZioStreams,
      RIO[R, *]
    ]
  )(implicit runtime: Runtime[R], mat: Materializer): ServerEndpoint[AkkaStreams, Future] =
    ServerEndpoint[
      Unit,
      Unit,
      Input,
      TapirResponse,
      (MediaType, Either[ResponseValue, AkkaStreams.BinaryStream]),
      AkkaStreams,
      Future
    ](
      endpoint.endpoint
        .asInstanceOf[
          PublicEndpoint[
            Input,
            TapirResponse,
            (MediaType, Either[ResponseValue, AkkaStreams.BinaryStream]),
            AkkaStreams
          ]
        ],
      _ => _ => Future.successful(Right(())),
      _ =>
        _ =>
          req =>
            Unsafe.unsafe { implicit u =>
              runtime.unsafe
                .runToFuture(
                  endpoint
                    .logic(zioMonadError)(())(req)
                    .right
                    .flatMap {
                      case (mediaType, Right(stream))  =>
                        ZIO
                          .succeed(Source.queue[ByteString](0, OverflowStrategy.fail).preMaterialize())
                          .flatMap { case (queue, source) =>
                            stream
                              .runForeachChunk(chunk => ZIO.fromFuture(_ => queue.offer(ByteString(chunk.toArray))))
                              .ensuring(ZIO.succeed(queue.complete()))
                              .forkDaemon
                              .flatMap(fiber =>
                                ZIO.executorWith(executor =>
                                  ZIO.succeed(
                                    (
                                      mediaType,
                                      Right(
                                        source
                                          .watchTermination()((_, f) =>
                                            f.onComplete(_ => runtime.unsafe.run(fiber.interrupt))(
                                              executor.asExecutionContext
                                            )
                                          )
                                      )
                                    )
                                  )
                                )
                              )
                          }
                          .asRightError
                      case (mediaType, Left(response)) =>
                        ZIO.succeed((mediaType, Left(response))).asRightError
                    }
                    .unright
                )
            }
    )

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
            Unsafe.unsafe { implicit u =>
              runtime.unsafe.runToFuture(
                endpoint
                  .logic(zioMonadError)(())(req)
                  .right
                  .flatMap { case (protocol, pipe) =>
                    val io =
                      for {
                        inputQueue     <- Queue.unbounded[GraphQLWSInput]
                        input           = ZStream.fromQueue(inputQueue)
                        output          = pipe(input)
                        ec             <- ZIO.executor.map(_.asExecutionContext)
                        sink            =
                          Sink.foreachAsync[GraphQLWSInput](1)(input =>
                            Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(inputQueue.offer(input).unit).future)
                          )
                        (queue, source) =
                          Source
                            .queue[Either[GraphQLWSClose, GraphQLWSOutput]](0, OverflowStrategy.fail)
                            .preMaterialize()
                        fiber          <- output.foreach(msg => ZIO.fromFuture(_ => queue.offer(msg))).forkDaemon
                        flow            = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() { (_, f) =>
                                            f.onComplete(_ => runtime.unsafe.run(fiber.interrupt).getOrThrowFiberFailure())(ec)
                                          }
                      } yield (protocol, flow)

                    io
                  }
                  .unright
              )
            }
    )
}
