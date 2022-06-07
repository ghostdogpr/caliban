package caliban

import akka.stream.{ Materializer, OverflowStrategy }
import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.util.ByteString
import caliban.PlayAdapter.convertHttpStreamingEndpoint
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, TapirResponse, ZioWebSockets }
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import play.api.routing.Router.Routes
import sttp.capabilities.WebSockets
import sttp.capabilities.akka.AkkaStreams
import sttp.capabilities.akka.AkkaStreams.Pipe
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.PublicEndpoint
import sttp.tapir.json.play._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.play.{ PlayServerInterpreter, PlayServerOptions }
import zio.{ RIO, Runtime, ZIO, ZQueue }
import zio.duration.Duration
import zio.random.Random
import zio.stream.ZStream

import scala.concurrent.{ ExecutionContext, Future }

class PlayAdapter private (private val options: Option[PlayServerOptions]) {
  private def playInterpreter(implicit mat: Materializer) =
    options.fold(PlayServerInterpreter())(PlayServerInterpreter(_))

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit runtime: Runtime[R], materializer: Materializer, ec: ExecutionContext): Routes = {
    val endpoints = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    playInterpreter.toRoutes(
      endpoints.map(endpoint =>
        convertHttpStreamingEndpoint(
          endpoint.asInstanceOf[
            ServerEndpoint.Full[Unit, Unit, ServerRequest, TapirResponse, ZioStreams.BinaryStream, ZioStreams, RIO[
              R,
              *
            ]]
          ]
        )
      )
    )
  }

  def makeHttpUploadService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit runtime: Runtime[R with Random], materializer: Materializer, ec: ExecutionContext): Routes = {
    val endpoint = TapirAdapter.makeHttpUploadService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    playInterpreter.toRoutes(
      convertHttpStreamingEndpoint(
        endpoint.asInstanceOf[
          ServerEndpoint.Full[Unit, Unit, ServerRequest, TapirResponse, ZioStreams.BinaryStream, ZioStreams, RIO[
            R,
            *
          ]]
        ]
      )
    )
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
    ec: ExecutionContext,
    runtime: Runtime[R],
    materializer: Materializer,
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): Routes = {
    val endpoint = TapirAdapter.makeWebSocketService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      keepAliveTime,
      queryExecution,
      requestInterceptor,
      webSocketHooks
    )
    playInterpreter.toRoutes(
      PlayAdapter.convertWebSocketEndpoint(
        endpoint.asInstanceOf[
          ServerEndpoint.Full[Unit, Unit, ServerRequest, StatusCode, CalibanPipe, ZioWebSockets, RIO[R, *]]
        ]
      )
    )
  }
}

object PlayAdapter extends PlayAdapter(None) {

  def apply(options: PlayServerOptions) =
    new PlayAdapter(Some(options))

  type AkkaPipe = Flow[GraphQLWSInput, GraphQLWSOutput, Any]

  def convertHttpStreamingEndpoint[R](
    endpoint: ServerEndpoint.Full[Unit, Unit, ServerRequest, TapirResponse, ZioStreams.BinaryStream, ZioStreams, RIO[
      R,
      *
    ]]
  )(implicit ec: ExecutionContext, runtime: Runtime[R], mat: Materializer): ServerEndpoint[AkkaStreams, Future] =
    ServerEndpoint[Unit, Unit, ServerRequest, TapirResponse, AkkaStreams.BinaryStream, AkkaStreams, Future](
      endpoint.endpoint
        .asInstanceOf[PublicEndpoint[ServerRequest, TapirResponse, AkkaStreams.BinaryStream, AkkaStreams]],
      _ => _ => Future.successful(Right(())),
      _ =>
        _ =>
          req =>
            runtime
              .unsafeRunToFuture(endpoint.logic(zioMonadError)(())(req))
              .future
              .map(_.map { stream =>
                val (queue, source) = Source.queue[ByteString](0, OverflowStrategy.fail).preMaterialize()
                runtime.unsafeRun(
                  stream
                    .foreachChunk(msg => ZIO.fromFuture(_ => queue.offer(ByteString(msg.toArray))))
                    .forkDaemon
                    .map(fiber =>
                      source.watchTermination()((_, f) => f.onComplete(_ => runtime.unsafeRun(fiber.interrupt)))
                    )
                )
              })
    )

  def convertWebSocketEndpoint[R](
    endpoint: ServerEndpoint.Full[Unit, Unit, ServerRequest, StatusCode, CalibanPipe, ZioWebSockets, RIO[R, *]]
  )(implicit
    ec: ExecutionContext,
    runtime: Runtime[R],
    materializer: Materializer
  ): ServerEndpoint[AkkaStreams with WebSockets, Future] =
    ServerEndpoint[Unit, Unit, ServerRequest, StatusCode, AkkaPipe, AkkaStreams with WebSockets, Future](
      endpoint.endpoint
        .asInstanceOf[PublicEndpoint[ServerRequest, StatusCode, Pipe[GraphQLWSInput, GraphQLWSOutput], Any]],
      _ => _ => Future.successful(Right(())),
      _ =>
        _ =>
          req =>
            runtime
              .unsafeRunToFuture(endpoint.logic(zioMonadError)(())(req))
              .future
              .map(_.map { zioPipe =>
                val io =
                  for {
                    inputQueue     <- ZQueue.unbounded[GraphQLWSInput]
                    input           = ZStream.fromQueue(inputQueue)
                    output          = zioPipe(input)
                    sink            = Sink.foreachAsync[GraphQLWSInput](1)(input =>
                                        runtime.unsafeRunToFuture(inputQueue.offer(input).unit).future
                                      )
                    (queue, source) = Source.queue[GraphQLWSOutput](0, OverflowStrategy.fail).preMaterialize()
                    fiber          <- output.foreach(msg => ZIO.fromFuture(_ => queue.offer(msg))).forkDaemon
                    flow            = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() { (_, f) =>
                                        f.onComplete(_ => runtime.unsafeRun(fiber.interrupt))
                                      }
                  } yield flow
                runtime.unsafeRun(io)
              })
    )
}
