package io.laminext.websocket

package object jsoniter extends ReExports {

  implicit def webSocketReceiveBuilderSyntax(b: WebSocketReceiveBuilder): WebSocketReceiveBuilderJsoniterOps =
    new WebSocketReceiveBuilderJsoniterOps(b)
}
