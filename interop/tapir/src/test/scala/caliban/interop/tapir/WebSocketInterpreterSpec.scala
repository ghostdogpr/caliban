package caliban.interop.tapir

import zio.test._

object WebSocketInterpreterSpec extends ZIOSpecDefault {
  override def spec = suite("WebSocketInterpreterSpec")(
    test("selects a single supported subprotocol from a multi-value header") {
      assertTrue(
        // preference: graphql-transport-ws wins even when listed with others
        WebSocketInterpreter.selectSubProtocol("graphql-transport-ws, graphql-ws").contains("graphql-transport-ws"),
        WebSocketInterpreter.selectSubProtocol("graphql-ws, graphql-transport-ws").contains("graphql-transport-ws"),
        WebSocketInterpreter.selectSubProtocol("graphql-ws").contains("graphql-ws"),
        WebSocketInterpreter.selectSubProtocol("graphql-transport-ws").contains("graphql-transport-ws"),
        // an unsupported token mixed with a supported one still selects the supported one
        WebSocketInterpreter.selectSubProtocol("foo, graphql-ws").contains("graphql-ws")
      )
    },
    test("does not select an unsupported subprotocol") {
      assertTrue(
        WebSocketInterpreter.selectSubProtocol("foo, bar").isEmpty,
        WebSocketInterpreter.selectSubProtocol("").isEmpty
      )
    }
  )
}
