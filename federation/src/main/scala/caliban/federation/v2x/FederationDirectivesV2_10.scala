package caliban.federation.v2x

import caliban.federation.connect
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

import scala.annotation.nowarn

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

  @nowarn("msg=deprecated")
  case class GQLConnect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String] = None,
    entity: Option[Boolean] = None
  ) extends GQLDirective(Connect(http, selection, source, entity))

  @nowarn("msg=deprecated")
  case class GQLSource(
    name: String,
    baseURL: String,
    headers: List[HTTPHeaderMapping] = Nil
  ) extends GQLDirective(Source(name, baseURL, headers))

  @deprecated("Use caliban.federation.connect.Connect instead", "3.0.1")
  def Connect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String],
    entity: Option[Boolean]
  ): Directive =
    connect.Connect(
      connect.ConnectHTTP(
        http.method match {
          case Method.GET(url)         => connect.Method.GET(url)
          case Method.DELETE(url)      => connect.Method.DELETE(url)
          case Method.POST(url, body)  => connect.Method.POST(url, body.map(sel => connect.JSONSelection(sel.select)))
          case Method.PUT(url, body)   => connect.Method.PUT(url, body.map(sel => connect.JSONSelection(sel.select)))
          case Method.PATCH(url, body) => connect.Method.PATCH(url, body.map(sel => connect.JSONSelection(sel.select)))
        },
        http.headers.map { h =>
          connect.HTTPHeaderMapping(name = h.name, from = h.from, value = h.value)
        }
      ),
      connect.JSONSelection(selection.select),
      source,
      entity
    )

  @deprecated("Use caliban.federation.connect.Source instead", "3.0.1")
  def Source(
    name: String,
    baseURL: String,
    headers: List[HTTPHeaderMapping] = Nil
  ): Directive =
    connect.Source(
      name,
      baseURL,
      headers.map(h => connect.HTTPHeaderMapping(name = h.name, from = h.from, value = h.value))
    )

}
