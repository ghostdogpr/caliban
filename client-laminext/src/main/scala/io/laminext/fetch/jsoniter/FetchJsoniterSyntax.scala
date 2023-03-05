package io.laminext.fetch.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{ writeToString, JsonValueCodec }

trait FetchJsoniterSyntax {

  implicit def jsonRequestBody[A](value: A)(implicit encoder: JsonValueCodec[A]): ToRequestBody =
    new JsonToRequestBody(writeToString(value))

  implicit def fetchEventStreamBuilderSyntaxCirce(b: FetchEventStreamBuilder): FetchEventStreamBuilderJsoniterOps =
    new FetchEventStreamBuilderJsoniterOps(b)
}
