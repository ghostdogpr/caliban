package caliban.federation

import caliban.{ InputValue, Value }
import caliban.parsing.adt.Directive

package object connect {

  def Connect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String],
    entity: Option[Boolean],
    batch: Option[BatchSettings] = None,
    errors: Option[ConnectorErrors] = None
  ): Directive = {
    val connectBuilder = Map.newBuilder[String, InputValue]
    val httpBuilder    = Map.newBuilder[String, InputValue]

    http.method match {
      case Method.GET(url)      => "GET"    -> Value.StringValue(url)
      case Method.DELETE(url)   => "DELETE" -> Value.StringValue(url)
      case Method.POST(url, _)  => "POST"   -> Value.StringValue(url)
      case Method.PUT(url, _)   => "PUT"    -> Value.StringValue(url)
      case Method.PATCH(url, _) => "PATCH"  -> Value.StringValue(url)
    }
    http.method.body.foreach(body => httpBuilder += "body" -> Value.StringValue(body.select))
    if (http.headers.nonEmpty)
      httpBuilder += "headers" -> InputValue.ListValue(
        http.headers.map(h =>
          InputValue.ObjectValue(
            Map(
              "name"  -> Value.StringValue(h.name),
              "from"  -> h.from.fold[InputValue](Value.NullValue)(from =>
                InputValue.ObjectValue(Map("from" -> Value.StringValue(from)))
              ),
              "value" -> h.value.fold[InputValue](Value.NullValue)(value =>
                InputValue.ObjectValue(Map("value" -> Value.StringValue(value)))
              )
            )
          )
        )
      )

    connectBuilder += "http"      -> InputValue.ObjectValue(httpBuilder.result())
    connectBuilder += "selection" -> Value.StringValue(selection.select)
    source.foreach(s => connectBuilder += "source" -> Value.StringValue(s))
    entity.foreach(e => connectBuilder += "entity" -> Value.BooleanValue(e))
    batch.foreach {
      case BatchSettings(Some(maxSize)) =>
        connectBuilder += "batch" -> InputValue.ObjectValue(Map("maxSize" -> Value.IntValue(maxSize)))
      case _                            => ()
    }
    errors.foreach { e =>
      if (e.message.isDefined || e.extensions.isDefined) {
        val mb = Map.newBuilder[String, InputValue]
        e.message.foreach(m => mb += ("message" -> Value.StringValue(m.select)))
        e.extensions.foreach(m => mb += ("extensions" -> Value.StringValue(m.select)))
        InputValue.ObjectValue(mb.result())
      }
    }

    Directive("source", httpBuilder.result())
  }

  def Source(
    name: String,
    baseURL: String,
    headers: List[HTTPHeaderMapping] = Nil
  ): Directive = {
    val sourceBuilder = Map.newBuilder[String, InputValue]
    sourceBuilder += "name"      -> Value.StringValue(name)
    sourceBuilder += "baseURL"   -> Value.StringValue(baseURL)
    if (headers.nonEmpty)
      sourceBuilder += "headers" -> InputValue.ListValue(
        headers.map(h =>
          InputValue.ObjectValue(
            Map(
              "name"  -> Value.StringValue(h.name),
              "from"  -> h.from.fold[InputValue](InputValue.ObjectValue(Map()))(from =>
                InputValue.ObjectValue(Map("from" -> Value.StringValue(from)))
              ),
              "value" -> h.value.fold[InputValue](InputValue.ObjectValue(Map()))(value =>
                InputValue.ObjectValue(Map("value" -> Value.StringValue(value)))
              )
            )
          )
        )
      )

    Directive("source", sourceBuilder.result())
  }

}
