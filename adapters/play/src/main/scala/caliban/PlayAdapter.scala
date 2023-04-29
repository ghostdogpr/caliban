package caliban

import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, OverflowStrategy }
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, ZioWebSockets }
import caliban.interop.tapir.{ HttpAdapter, HttpUploadAdapter, TapirAdapter, WebSocketAdapter }
import play.api.routing.Router.Routes
import sttp.capabilities.WebSockets
import sttp.capabilities.akka.AkkaStreams
import sttp.capabilities.akka.AkkaStreams.Pipe
import sttp.model.StatusCode
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.PublicEndpoint
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.play.{ PlayServerInterpreter, PlayServerOptions }
import zio._
import zio.stream.ZStream

import scala.concurrent.{ ExecutionContext, Future }

class PlayAdapter private (private val options: Option[PlayServerOptions]) {
  private def playInterpreter(implicit mat: Materializer) =
    options.fold(PlayServerInterpreter())(PlayServerInterpreter(_))

  def makeHttpService[R, E](
    adapter: HttpAdapter[R, E]
  )(implicit runtime: Runtime[R], materializer: Materializer): Routes =
    playInterpreter.toRoutes(adapter.serverEndpoints[R].map(TapirAdapter.convertHttpEndpointToFuture(_)))

  def makeHttpUploadService[R, E](adapter: HttpUploadAdapter[R, E])(implicit
    runtime: Runtime[R],
    materializer: Materializer,
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): Routes =
    playInterpreter.toRoutes(TapirAdapter.convertHttpEndpointToFuture(adapter.serverEndpoint[R]))

  def makeWebSocketService[R, E](
    adapter: WebSocketAdapter[R, E]
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): Routes = {
    val endpoint = adapter.serverEndpoint[R]
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
                    sink            =
                      Sink.foreachAsync[GraphQLWSInput](1)(input =>
                        Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(inputQueue.offer(input).unit).future)
                      )
                    (queue, source) =
                      Source.queue[Either[GraphQLWSClose, GraphQLWSOutput]](0, OverflowStrategy.fail).preMaterialize()
                    fiber          <- output.foreach(msg => ZIO.fromFuture(_ => queue.offer(msg))).forkDaemon
                    flow            = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() { (_, f) =>
                                        f.onComplete(_ =>
                                          Unsafe.unsafe(implicit u => runtime.unsafe.run(fiber.interrupt).getOrThrowFiberFailure())
                                        )
                                      }
                  } yield (protocol, flow)
                Unsafe.unsafe(implicit u => runtime.unsafe.run(io).getOrThrowFiberFailure())
              })
    )
}
