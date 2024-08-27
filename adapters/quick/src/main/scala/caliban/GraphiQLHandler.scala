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
   *
   * @see [[https://github.com/graphql/graphiql/tree/main/examples/graphiql-cdn]]
   */
  def handler(apiPath: String): RequestHandler[Any, Nothing] = {
    val headers = Headers(Header.ContentType(MediaType.text.html).untyped)
    zio.http.handler { (req: Request) =>
      Response(
        Status.Ok,
        headers,
        Body.fromString(html(apiPath, req.path.encode))
      )
    }
  }

  @deprecated("Use overloaded method without providing the graphiqlPath param", since = "2.8.2")
  def handler(apiPath: String, graphiqlPath: String): RequestHandler[Any, Nothing] =
    Response(
      Status.Ok,
      Headers(Header.ContentType(MediaType.text.html).untyped),
      Body.fromString(html(apiPath, graphiqlPath))
    ).toHandler

  def html(apiPath: String, uiPath: String): String = HttpUtils.graphiqlHtml(apiPath, uiPath)
}
