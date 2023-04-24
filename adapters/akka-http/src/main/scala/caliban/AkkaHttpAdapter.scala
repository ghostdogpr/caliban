package caliban

import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, OverflowStrategy }
import caliban.AkkaHttpAdapter.convertWebSocketEndpoint
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter.{ zioMonadError, CalibanPipe, TapirResponse, ZioWebSockets }
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
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

  def makeHttpService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    runtime: Runtime[R1],
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): Route = {
    val endpoints = TapirAdapter.makeHttpService[R1, R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    akkaInterpreter.toRoute(endpoints.map(TapirAdapter.convertHttpEndpointToFuture(_)))
  }

  def makeHttpUploadService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    runtime: Runtime[R1],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): Route = {
    val endpoint = TapirAdapter.makeHttpUploadService[R1, R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    akkaInterpreter.toRoute(TapirAdapter.convertHttpEndpointToFuture(endpoint))
  }

  def makeWebSocketService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit
    ec: ExecutionContext,
    runtime: Runtime[R1],
    materializer: Materializer,
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): Route = {
    val endpoint = TapirAdapter.makeWebSocketService[R1, R, E](
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
            RIO[R1, *]
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
