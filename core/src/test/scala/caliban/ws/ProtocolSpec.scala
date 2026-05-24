package caliban.ws

import caliban._
import caliban.schema.Schema.auto._
import zio._
import zio.stream.ZStream
import zio.test._

object ProtocolSpec extends ZIOSpecDefault {

  private case class Query(secret: String)
  private case class Subscriptions(loop: ZStream[Any, Nothing, Int])

  private val api         = graphQL(
    RootResolver(Some(Query("TOP_SECRET")), Option.empty[Unit], Some(Subscriptions(ZStream.never)))
  )
  private val interpreter = api.interpreterUnsafe

  private val rejectAll: WebSocketHooks[Any, CalibanError] =
    WebSocketHooks.init[Any, CalibanError](_ => ZIO.fail(CalibanError.ExecutionError("auth required")))

  private val partialFnHook: WebSocketHooks[Any, CalibanError] =
    WebSocketHooks.init[Any, CalibanError] { v =>
      (v: @unchecked) match { case InputValue.ObjectValue(_) => ZIO.unit }
    }

  private val initNoPayload = ZStream(GraphQLWSInput("connection_init", None, None))
  private val subscribeMsg  =
    GraphQLWSInput(
      "subscribe",
      Some("1"),
      Some(InputValue.ObjectValue(Map("query" -> Value.StringValue("{ secret }"))))
    )
  private val startMsg      =
    GraphQLWSInput("start", Some("1"), Some(InputValue.ObjectValue(Map("query" -> Value.StringValue("{ secret }")))))

  override def spec = suite("ProtocolSpec")(
    suite("GraphQLWS (graphql-transport-ws)")(
      test("beforeInit is invoked with NullValue when connection_init has no payload") {
        for {
          called <- Ref.make(Option.empty[InputValue])
          hooks   = WebSocketHooks.init[Any, CalibanError](v => called.set(Some(v)))
          pipe   <- Protocol.GraphQLWS.make(interpreter, None, hooks)
          _      <- pipe(initNoPayload).take(1).runDrain
          seen   <- called.get
        } yield assertTrue(seen.contains(Value.NullValue))
      },
      test("beforeInit receives the original payload when one is provided") {
        for {
          called <- Ref.make(Option.empty[InputValue])
          hooks   = WebSocketHooks.init[Any, CalibanError](v => called.set(Some(v)))
          payload = InputValue.ObjectValue(Map("token" -> Value.StringValue("abc")))
          pipe   <- Protocol.GraphQLWS.make(interpreter, None, hooks)
          _      <- pipe(ZStream(GraphQLWSInput("connection_init", None, Some(payload)))).take(1).runDrain
          seen   <- called.get
        } yield assertTrue(seen.contains(payload))
      },
      test("beforeInit rejection: 4403 close is the only frame and ack stays false (subscribe gets 4401)") {
        for {
          pipe <- Protocol.GraphQLWS.make(interpreter, None, rejectAll)
          out  <- pipe(ZStream(GraphQLWSInput("connection_init", None, None), subscribeMsg)).take(2).runCollect
        } yield assertTrue(
          out == Chunk(
            Left(GraphQLWSClose(4403, "Forbidden")),
            Left(GraphQLWSClose(4401, "Unauthorized"))
          )
        )
      },
      test("beforeInit defect (MatchError on NullValue) is recovered as 4403 close, not a silent fiber death") {
        for {
          pipe <- Protocol.GraphQLWS.make(interpreter, None, partialFnHook)
          out  <- pipe(ZStream(GraphQLWSInput("connection_init", None, None), subscribeMsg)).take(2).runCollect
        } yield assertTrue(
          out == Chunk(
            Left(GraphQLWSClose(4403, "Forbidden")),
            Left(GraphQLWSClose(4401, "Unauthorized"))
          )
        )
      },
      test("duplicate subscribe with the same id is rejected with 4409 (issue #2973)") {
        val dup = GraphQLWSInput(
          "subscribe",
          Some("dup"),
          Some(InputValue.ObjectValue(Map("query" -> Value.StringValue("subscription { loop }"))))
        )
        for {
          pipe <- Protocol.GraphQLWS.make(interpreter, None, WebSocketHooks.empty[Any, CalibanError])
          out  <- pipe(ZStream(GraphQLWSInput("connection_init", None, None), dup, dup)).collect { case Left(close) =>
                    close
                  }
                    .take(1)
                    .runHead
        } yield assertTrue(out.contains(GraphQLWSClose(4409, "Subscriber for dup already exists")))
      },
      test("duplicate connection_init after a successful one is rejected with 4429") {
        for {
          pipe <- Protocol.GraphQLWS.make(interpreter, None, WebSocketHooks.empty[Any, CalibanError])
          out  <- pipe(
                    ZStream(
                      GraphQLWSInput("connection_init", None, None),
                      GraphQLWSInput("connection_init", None, None)
                    )
                  ).collect { case Left(close) => close }.take(1).runHead
        } yield assertTrue(out.contains(GraphQLWSClose(4429, "Too many initialisation requests")))
      },
      test("composed beforeInit hooks (++) both run with NullValue on payload-less init") {
        for {
          calls <- Ref.make(List.empty[String])
          h1     = WebSocketHooks.init[Any, CalibanError](_ => calls.update("h1" :: _))
          h2     = WebSocketHooks.init[Any, CalibanError](_ => calls.update("h2" :: _))
          pipe  <- Protocol.GraphQLWS.make(interpreter, None, h1 ++ h2)
          _     <- pipe(initNoPayload).take(1).runDrain
          seen  <- calls.get
        } yield assertTrue(seen.toSet == Set("h1", "h2"))
      }
    ),
    suite("Legacy (graphql-ws)")(
      test("beforeInit is invoked with NullValue when connection_init has no payload") {
        for {
          called <- Ref.make(Option.empty[InputValue])
          hooks   = WebSocketHooks.init[Any, CalibanError](v => called.set(Some(v)))
          pipe   <- Protocol.Legacy.make(interpreter, None, hooks)
          _      <- pipe(initNoPayload).take(1).runDrain
          seen   <- called.get
        } yield assertTrue(seen.contains(Value.NullValue))
      },
      test("beforeInit rejection emits a connection_error frame (with payload) and ack stays false (start gets 4401)") {
        for {
          pipe <- Protocol.Legacy.make(interpreter, None, rejectAll)
          out  <- pipe(ZStream(GraphQLWSInput("connection_init", None, None), startMsg)).take(2).runCollect
        } yield assertTrue(
          out.size == 2,
          out(0) match {
            case Right(GraphQLWSOutput("connection_error", None, Some(_))) => true
            case _                                                         => false
          },
          out(1) == Left(GraphQLWSClose(4401, "Unauthorized"))
        )
      },
      test("beforeInit defect emits a connection_error frame and ack stays false") {
        for {
          pipe <- Protocol.Legacy.make(interpreter, None, partialFnHook)
          out  <- pipe(ZStream(GraphQLWSInput("connection_init", None, None), startMsg)).take(2).runCollect
        } yield assertTrue(
          out.size == 2,
          out(0) == Right(GraphQLWSOutput("connection_error", None, None)),
          out(1) == Left(GraphQLWSClose(4401, "Unauthorized"))
        )
      },
      test("duplicate connection_init after a successful one emits a connection_error frame") {
        for {
          pipe <- Protocol.Legacy.make(interpreter, None, WebSocketHooks.empty[Any, CalibanError])
          out  <- pipe(
                    ZStream(
                      GraphQLWSInput("connection_init", None, None),
                      GraphQLWSInput("connection_init", None, None)
                    )
                  ).take(2).runCollect
        } yield assertTrue(
          out.size == 2,
          out(0) match {
            case Right(GraphQLWSOutput("connection_ack", _, _)) => true
            case _                                              => false
          },
          out(1) == Right(GraphQLWSOutput("connection_error", None, None))
        )
      },
      test("two starts with same id, then stop then terminate, completes without hanging") {
        val start = GraphQLWSInput(
          "start",
          Some("s"),
          Some(InputValue.ObjectValue(Map("query" -> Value.StringValue("subscription { loop }"))))
        )
        for {
          pipe <- Protocol.Legacy.make(interpreter, None, WebSocketHooks.empty[Any, CalibanError])
          _    <- pipe(
                    ZStream(
                      GraphQLWSInput("connection_init", None, None),
                      start,
                      start,
                      GraphQLWSInput("stop", Some("s"), None),
                      GraphQLWSInput("connection_terminate", None, None)
                    )
                  ).runDrain.timeoutFail(new RuntimeException("output did not terminate"))(5.seconds)
        } yield assertCompletes
      },
      test("connection_terminate shuts down the output stream so the WS closes (issue #2974)") {
        for {
          pipe <- Protocol.Legacy.make(interpreter, None, WebSocketHooks.empty[Any, CalibanError])
          _    <- pipe(
                    ZStream(
                      GraphQLWSInput("connection_init", None, None),
                      GraphQLWSInput("connection_terminate", None, None)
                    )
                  ).runDrain.timeoutFail(new RuntimeException("output stream did not terminate"))(5.seconds)
        } yield assertCompletes
      }
    )
  )
}
