package caliban

import zio.Trace
import zio.http._
import zio.stacktracer.TracingImplicits.disableAutoTrace

object GraphiQLHandler {
  private implicit val trace: Trace = Trace.empty

  /**
   * Creates a handler which serves the GraphiQL UI from CDN.
   *
   * @param apiPath The path at which the API can be introspected.
   * @param wsPath The path at which the WS subscription can be introspected.
   *
   * @see [[https://github.com/graphql/graphiql/tree/main/examples/graphiql-cdn]]
   */
  def handler(apiPath: String, wsPath: Option[String]): RequestHandler[Any, Nothing] = {
    val headers = Headers(Header.ContentType(MediaType.text.html).untyped)
    zio.http.handler { (req: Request) =>
      val body = wsPath.fold(html(apiPath, req.path.encode))(html(apiPath, req.path.encode, _))
      Response(
        Status.Ok,
        headers,
        Body.fromString(body)
      )
    }
  }

  def html(apiPath: String, uiPath: String): String =
    HttpUtils.graphiqlHtml(apiPath, uiPath, wsPath = None)

  def html(apiPath: String, uiPath: String, wsPath: String): String =
    HttpUtils.graphiqlHtml(apiPath, uiPath, Some(wsPath))
}
