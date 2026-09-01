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

  def spec = suite("Subscription transports")(
    test("SSE preserves each complete event's errors and extensions and then completes") {
      val failed   = GraphQLResponse(Value.NullValue, List(CalibanError.ExecutionError("bad event")))
      val next     = GraphQLResponse(
        ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))),
        Nil,
        Some(ResponseValue.ObjectValue(List("trace" -> Value.StringValue("next"))))
      )
      val response =
        GraphQLResponse(ResponseValue.StreamValue(ZStream(failed.toResponseValue, next.toResponseValue)), Nil)
      val complete = Value.StringValue("complete")
      HttpUtils.ServerSentEvents
        .transformResponse(response, identity[ResponseValue], complete)
        .runCollect
        .map(events => assertTrue(events == Chunk(failed.toResponseValue, next.toResponseValue, complete)))
    },
    test("SSE emits wrapper errors once before top-level complete-envelope streams") {
      val wrapperError = CalibanError.ExecutionError("wrapper failed")
      val first        = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(1))), Nil)
      val second       = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))), Nil)
      val response     = GraphQLResponse(
        ResponseValue.StreamValue(ZStream(first.toResponseValue, second.toResponseValue)),
        List(wrapperError)
      )
      val complete     = Value.StringValue("complete")
      HttpUtils.ServerSentEvents
        .transformResponse(response, identity[ResponseValue], complete)
        .runCollect
        .map(events =>
          assertTrue(
            events == Chunk(
              GraphQLResponse(Value.NullValue, List(wrapperError)).toResponseValue,
              first.toResponseValue,
              second.toResponseValue,
              complete
            )
          )
        )
    },
    test("SSE terminal errors use the existing error representation and then complete") {
      val failure  = CalibanError.ExecutionError("resubscribe")
      val response = GraphQLResponse(ResponseValue.StreamValue(ZStream.fail(failure)), Nil)
      val complete = Value.StringValue("complete")
      HttpUtils.ServerSentEvents
        .transformResponse(response, identity[ResponseValue], complete)
        .runCollect
        .map(events =>
          assertTrue(events == Chunk(GraphQLResponse(Value.NullValue, List(failure)).toResponseValue, complete))
        )
    },
    test("modern WebSocket terminal errors are not followed by complete") {
      val failure = CalibanError.ExecutionError("resubscribe")
      ZIO.scoped {
        for {
          input      <- Queue.unbounded[GraphQLWSInput]
          output     <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
          interpreter = new GraphQLInterpreter[Any, CalibanError] {
                          def check(query: String)(implicit trace: Trace)                    = ZIO.unit
                          def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = ZIO.succeed(
                            GraphQLResponse(ResponseValue.StreamValue(ZStream.fail(failure)), Nil)
                          )
                        }
          pipe       <- ws.Protocol.GraphQLWS.make(interpreter, None, ws.WebSocketHooks.empty[Any, CalibanError])
          _          <- pipe(ZStream.fromQueue(input)).runForeach(output.offer).forkScoped
          _          <- input.offer(init)
          _          <- output.take
          _          <- input.offer(subscribe("1"))
          error      <- Live.live(output.take.timeout(2.seconds))
          _          <- input.offer(GraphQLWSInput("ping", None, None))
          next       <- Live.live(output.take.timeout(2.seconds))
        } yield assertTrue(
          error.exists(
            _.exists(v =>
              v.`type` == "error" && v.id.contains("1") && v.payload.exists(_.toString.contains("resubscribe"))
            )
          ),
          next.exists(_.exists(_.`type` == "pong"))
        )
      }
    },
    test("modern WebSocket emits wrapper errors once before top-level complete-envelope streams") {
      val wrapperError = CalibanError.ExecutionError("wrapper failed")
      val first        = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(1))), Nil)
      val second       = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))), Nil)
      ZIO.scoped {
        for {
          input      <- Queue.unbounded[GraphQLWSInput]
          output     <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
          interpreter = new GraphQLInterpreter[Any, CalibanError] {
                          def check(query: String)(implicit trace: Trace)                    = ZIO.unit
                          def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = ZIO.succeed(
                            GraphQLResponse(
                              ResponseValue.StreamValue(ZStream(first.toResponseValue, second.toResponseValue)),
                              List(wrapperError)
                            )
                          )
                        }
          pipe       <- ws.Protocol.GraphQLWS.make(interpreter, None, ws.WebSocketHooks.empty[Any, CalibanError])
          _          <- pipe(ZStream.fromQueue(input)).runForeach(output.offer).forkScoped
          _          <- input.offer(init)
          _          <- output.take
          _          <- input.offer(subscribe("1"))
          wrapper    <- output.take
          next       <- output.take
          following  <- output.take
        } yield assertTrue(
          wrapper.exists(output =>
            output.payload.contains(GraphQLResponse(Value.NullValue, List(wrapperError)).toResponseValue)
          ),
          next.exists(_.payload.contains(first.toResponseValue)),
          following.exists(_.payload.contains(second.toResponseValue))
        )
      }
    },
    test("two operations on one socket keep their stream layers alive and release them independently") {
      final case class Resource(id: String)
      ZIO.scoped {
        for {
          closed         <- Ref.make(Set.empty[String])
          opened         <- Ref.make(Set.empty[String])
          input          <- Queue.unbounded[GraphQLWSInput]
          output         <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
          operationEvents = (request: GraphQLRequest) => {
                              val id    = request.operationName.get
                              val layer = ZLayer.scoped(
                                ZIO.acquireRelease(opened.update(_ + id).as(Resource(id)))(_ => closed.update(_ + id))
                              )
                              (ZStream.fromZIO(
                                ZIO.service[Resource].map(r => GraphQLResponse(Value.StringValue(r.id), Nil))
                              ) ++ ZStream.never)
                                .provideLayer(layer)
                            }
          interpreter     = new GraphQLInterpreter[Any, String] {
                              def check(query: String)(implicit trace: Trace)                    = ZIO.unit
                              def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = ZIO.succeed(
                                GraphQLResponse(
                                  ResponseValue.StreamValue(operationEvents(request).map(_.toResponseValue)),
                                  Nil
                                )
                              )
                            }
          pipe           <- ws.Protocol.GraphQLWS.make(interpreter, None, ws.WebSocketHooks.empty[Any, String])
          fiber          <- pipe(ZStream.fromQueue(input)).runForeach(output.offer).forkScoped
          _              <- input.offer(init)
          _              <- output.take
          _              <- input.offer(subscribe("first"))
          _              <- input.offer(subscribe("second"))
          _              <- output.take.repeatN(1)
          before         <- closed.get
          _              <- input.offer(GraphQLWSInput("complete", Some("first"), None))
          after          <- closed.get.repeatUntil(_.contains("first"))
          _              <- input.offer(GraphQLWSInput("ping", None, None))
          pong           <- output.take.repeatUntil(_.exists(_.`type` == "pong"))
          _              <- input.offer(GraphQLWSInput("complete", Some("second"), None))
          all            <- closed.get.repeatUntil(_.size == 2)
          _              <- fiber.interrupt
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
