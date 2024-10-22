package caliban.federation.v2x

import caliban.{ InputValue, Value }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_10 extends FederationDirectivesV2_9 {

  case class JSONSelection(select: String)
  case class HTTPHeaderMapping(
    name: String,
    from: Option[String],
    value: Option[String]
  )

  sealed trait Method {
    def url: String
    def body: Option[JSONSelection] = None
  }

  object Method {
    case class GET(url: String)                                             extends Method
    case class DELETE(url: String)                                          extends Method
    case class POST(url: String, override val body: Option[JSONSelection])  extends Method
    case class PUT(url: String, override val body: Option[JSONSelection])   extends Method
    case class PATCH(url: String, override val body: Option[JSONSelection]) extends Method
  }

  case class ConnectHTTP(
    method: Method,
    headers: List[HTTPHeaderMapping] = Nil
  )

  def Connect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String],
    entity: Option[Boolean]
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

    Directive("source", httpBuilder.result())
  }

  case class GQLConnect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String] = None,
    entity: Option[Boolean] = None
  ) extends GQLDirective(Connect(http, selection, source, entity))

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

  case class GQLSource(
    name: String,
    baseURL: String,
    headers: List[HTTPHeaderMapping] = Nil
  ) extends GQLDirective(Source(name, baseURL, headers))

}
