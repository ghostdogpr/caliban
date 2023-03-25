package io.laminext.websocket

import scala.language.implicitConversions

package object jsoniter extends ReExports {

  implicit def webSocketReceiveBuilderSyntax(b: WebSocketReceiveBuilder): WebSocketReceiveBuilderJsoniterOps =
    new WebSocketReceiveBuilderJsoniterOps(b)
}
