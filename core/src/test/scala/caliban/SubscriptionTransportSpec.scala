package caliban

import zio._
import zio.stream.ZStream
import zio.test._

object SubscriptionTransportSpec extends ZIOSpecDefault {
  private val init                  = GraphQLWSInput("connection_init", None, None)
  private def subscribe(id: String) = GraphQLWSInput(
    "subscribe",
    Some(id),
    Some(
      InputValue.ObjectValue(
        Map(
          "query"         -> Value.StringValue("subscription { event }"),
          "operationName" -> Value.StringValue(id)
        )
      )
    )
  )

  private val complete = Value.StringValue("complete")

  private def sse(response: GraphQLResponse[Any]) =
    HttpUtils.ServerSentEvents.transformResponse(response, identity[ResponseValue], complete).runCollect

  private final case class Socket(
    input: Queue[GraphQLWSInput],
    output: Queue[Either[GraphQLWSClose, GraphQLWSOutput]],
    fiber: Fiber[Any, Unit]
  )

  private def connect[E](respond: GraphQLRequest => GraphQLResponse[E]): ZIO[Scope, Nothing, Socket] = {
    val interpreter = new GraphQLInterpreter[Any, E] {
      def check(query: String)(implicit trace: Trace)                    = ZIO.unit
      def executeRequest(request: GraphQLRequest)(implicit trace: Trace) =
        GraphQLResponseContext.markSubscribed.as(respond(request))
    }
    for {
      input  <- Queue.unbounded[GraphQLWSInput]
      output <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
      pipe   <- ws.Protocol.GraphQLWS.make(interpreter, None, ws.WebSocketHooks.empty[Any, E])
      fiber  <- pipe(ZStream.fromQueue(input)).runForeach(output.offer).forkScoped
      _      <- input.offer(init) *> output.take
    } yield Socket(input, output, fiber)
  }

  def spec = suite("Subscription transports")(
    test(
      "SSE emits wrapper errors once, then each complete event with its errors and extensions, then a terminal error and complete"
    ) {
      val wrapperError = CalibanError.ExecutionError("wrapper failed")
      val failure      = CalibanError.ExecutionError("resubscribe")
      val failed       = GraphQLResponse(Value.NullValue, List(CalibanError.ExecutionError("bad event")))
      val next         = GraphQLResponse(
        ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))),
        Nil,
        Some(ResponseValue.ObjectValue(List("trace" -> Value.StringValue("next"))))
      )
      val response     = GraphQLResponse(
        ResponseValue.StreamValue(ZStream(failed.toResponseValue, next.toResponseValue) ++ ZStream.fail(failure)),
        List(wrapperError)
      )
      sse(response).map(events =>
        assertTrue(
          events == Chunk(
            GraphQLResponse(Value.NullValue, List(wrapperError)).toResponseValue,
            failed.toResponseValue,
            next.toResponseValue,
            GraphQLResponse(Value.NullValue, List(failure)).toResponseValue,
            complete
          )
        )
      )
    },
    test(
      "modern WebSocket emits wrapper errors once before top-level complete-envelope streams, and no complete after a terminal error"
    ) {
      val wrapperError = CalibanError.ExecutionError("wrapper failed")
      val failure      = CalibanError.ExecutionError("resubscribe")
      val first        = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(1))), Nil)
      val second       = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))), Nil)
      val stream       =
        ResponseValue.StreamValue(ZStream(first.toResponseValue, second.toResponseValue) ++ ZStream.fail(failure))
      ZIO.scoped {
        for {
          socket    <- connect(_ => GraphQLResponse(stream, List(wrapperError)))
          _         <- socket.input.offer(subscribe("1"))
          wrapper   <- socket.output.take
          next      <- socket.output.take
          following <- socket.output.take
          error     <- socket.output.take
          _         <- socket.input.offer(GraphQLWSInput("ping", None, None))
          pong      <- socket.output.take
        } yield assertTrue(
          wrapper.exists(output =>
            output.payload.contains(GraphQLResponse(Value.NullValue, List(wrapperError)).toResponseValue)
          ),
          next.exists(_.payload.contains(first.toResponseValue)),
          following.exists(_.payload.contains(second.toResponseValue)),
          error.exists(v =>
            v.`type` == "error" && v.id.contains("1") && v.payload.exists(_.toString.contains("resubscribe"))
          ),
          pong.exists(_.`type` == "pong")
        )
      }
    },
    test("two operations on one socket keep their stream layers alive and release them independently") {
      final case class Resource(id: String)
      ZIO.scoped {
        for {
          closed         <- Ref.make(Set.empty[String])
          opened         <- Ref.make(Set.empty[String])
          operationEvents = (request: GraphQLRequest) => {
                              val id    = request.operationName.getOrElse("")
                              val layer = ZLayer.scoped(
                                ZIO.acquireRelease(opened.update(_ + id).as(Resource(id)))(_ => closed.update(_ + id))
                              )
                              (ZStream.fromZIO(
                                ZIO.service[Resource].map(r => GraphQLResponse(Value.StringValue(r.id), Nil))
                              ) ++ ZStream.never)
                                .provideLayer(layer)
                            }
          socket         <- connect[String](request =>
                              GraphQLResponse(
                                ResponseValue.StreamValue(operationEvents(request).map(_.toResponseValue)),
                                Nil
                              )
                            )
          _              <- socket.input.offer(subscribe("first"))
          _              <- socket.input.offer(subscribe("second"))
          _              <- socket.output.take.repeatN(1)
          before         <- closed.get
          _              <- socket.input.offer(GraphQLWSInput("complete", Some("first"), None))
          after          <- closed.get.repeatUntil(_.contains("first"))
          _              <- socket.input.offer(GraphQLWSInput("ping", None, None))
          pong           <- socket.output.take.repeatUntil(_.exists(_.`type` == "pong"))
          _              <- socket.input.offer(GraphQLWSInput("complete", Some("second"), None))
          all            <- closed.get.repeatUntil(_.size == 2)
          _              <- socket.fiber.interrupt
        } yield assertTrue(
          before.isEmpty,
          after == Set("first"),
          all == Set("first", "second"),
          pong.exists(_.`type` == "pong")
        )
      }
    }
  ) @@ TestAspect.timeout(30.seconds)
}
