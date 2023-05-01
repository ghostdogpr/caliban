package caliban

import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, OverflowStrategy }
import akka.util.ByteString
import caliban.AkkaHttpAdapter.{ convertHttpStreamingEndpoint, convertWebSocketEndpoint }
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter._
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import sttp.capabilities.WebSockets
import sttp.capabilities.akka.AkkaStreams
import sttp.capabilities.akka.AkkaStreams.Pipe
import sttp.capabilities.zio.ZioStreams
import sttp.model.{ MediaType, StatusCode }
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

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    runtime: Runtime[R],
    materializer: Materializer,
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[ResponseValue]
  ): Route = {
    val endpoints: List[CalibanEndpoint[R]] = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    akkaInterpreter.toRoute(endpoints.map(convertHttpStreamingEndpoint[R, (GraphQLRequest, ServerRequest)]))
  }

  def makeHttpUploadService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    runtime: Runtime[R],
    materializer: Materializer,
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[ResponseValue]
  ): Route = {
    val endpoint = TapirAdapter.makeHttpUploadService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    akkaInterpreter.toRoute(convertHttpStreamingEndpoint(endpoint))
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit
    runtime: Runtime[R],
    materializer: Materializer,
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): Route = {
    val endpoint = TapirAdapter.makeWebSocketService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      keepAliveTime,
      queryExecution,
      requestInterceptor,
      webSocketHooks
    )
    akkaInterpreter.toRoute(
      convertWebSocketEndpoint(
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

object AkkaHttpAdapter {

  def default(implicit ec: ExecutionContext): AkkaHttpAdapter =
    apply(AkkaHttpServerOptions.default)

  def apply(options: AkkaHttpServerOptions)(implicit ec: ExecutionContext): AkkaHttpAdapter =
    new AkkaHttpAdapter(options)

  type AkkaPipe = Flow[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput], Any]

  def convertHttpStreamingEndpoint[R, I](
    endpoint: ServerEndpoint.Full[
      Unit,
      Unit,
      I,
      TapirResponse,
      CalibanResponse,
      ZioStreams,
      RIO[R, *]
    ]
  )(implicit runtime: Runtime[R], mat: Materializer): ServerEndpoint[AkkaStreams, Future] =
    ServerEndpoint[
      Unit,
      Unit,
      I,
      TapirResponse,
      (MediaType, Either[ResponseValue, AkkaStreams.BinaryStream]),
      AkkaStreams,
      Future
    ](
      endpoint.endpoint
        .asInstanceOf[
          PublicEndpoint[
            I,
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
                      case (mediaType, Right(stream)) =>
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
                      case (mediaType, Left(value))   =>
                        ZIO
                          .succeed(
                            (
                              mediaType,
                              Left(value)
                            )
                          )
                          .asRightError
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
