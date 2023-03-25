package io.laminext.websocket.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromString, writeToString, JsonValueCodec }
import io.laminext.websocket.{ initialize, receive, send, WebSocketBuilder, WebSocketReceiveBuilder }

import scala.util.control.NonFatal

class WebSocketReceiveBuilderJsoniterOps(b: WebSocketReceiveBuilder) {

  @inline def json[Receive, Send](implicit
    receiveCodec: JsonValueCodec[Receive],
    sendCodec: JsonValueCodec[Send]
  ): WebSocketBuilder[Receive, Send] =
    new WebSocketBuilder[Receive, Send](
      url = b.url,
      protocol = b.protocol,
      initializer = initialize.text,
      sender = send.text[Send](writeToString(_)),
      receiver = receive.text[Receive] { text =>
        try Right(readFromString[Receive](text))
        catch {
          case NonFatal(e) => Left(e)
        }
      }
    )
}
