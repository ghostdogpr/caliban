package caliban.gateway.internal

import caliban.gateway.RemoteGraphQLConfig
import caliban.gateway.internal.RemoteTransport.{ GraphQLResponseJson, Json }
import zio.{ durationInt, Scope, Task, Trace, ZIO, ZLayer }
import zio.http._
import zio.http.netty.NettyConfig

import java.nio.charset.StandardCharsets

private[gateway] final class GatewayHttpClient(client: Client) {
  import GatewayHttpClient._

  def post(url: URL, body: Array[Byte], headers: List[Header], maxResponseBytes: Int)(implicit
    trace: Trace
  ): Task[Reply] =
    send(Request.post(url, Body.fromArray(body)).addHeaders(jsonHeaders), headers, maxResponseBytes)

  def get(url: URL, headers: List[Header], maxResponseBytes: Int)(implicit trace: Trace): Task[Reply] =
    send(Request.get(url), headers, maxResponseBytes)

  def stream(request: Request, headers: List[Header])(implicit trace: Trace): ZIO[Scope, Throwable, Response] =
    client(request.addHeaders(joinDuplicates(headers)))

  def socket(url: URL, headers: List[Header], app: WebSocketApp[Any])(implicit
    trace: Trace
  ): ZIO[Scope, Throwable, Response] =
    client.url(url).addHeaders(joinDuplicates(headers)).socket(app)

  private def send(request: Request, headers: List[Header], maxResponseBytes: Int)(implicit trace: Trace): Task[Reply] =
    ZIO.scoped {
      client(request.addHeaders(joinDuplicates(headers))).flatMap { response =>
        def reply(body: Option[Array[Byte]]) = Reply(response.status, response.headers, body)
        response.header(Header.ContentLength).map(_.length) match {
          case Some(length) if length > maxResponseBytes => ZIO.succeed(reply(None))
          case Some(_)                                   => response.body.asArray.map(bytes => reply(Some(bytes)))
          case None                                      =>
            response.body.asStream
              .take(maxResponseBytes.toLong + 1L)
              .runCollect
              .map(bytes => reply(if (bytes.length > maxResponseBytes) None else Some(bytes.toArray)))
        }
      }
    }
}

private[gateway] object GatewayHttpClient {
  def make(implicit trace: Trace): ZIO[Scope, Throwable, GatewayHttpClient] =
    layer.build.map(env => new GatewayHttpClient(env.get[Client]))

  // `body` is None when the response exceeds maxResponseBytes.
  final case class Reply(status: Status, headers: Headers, body: Option[Array[Byte]]) {
    def contentType: Option[String] = headers.rawHeader(Header.ContentType)
  }

  val jsonContentType =
    Header.ContentType(MediaType.application.json, charset = Some(StandardCharsets.UTF_8))

  private val jsonHeaders = Headers(
    jsonContentType,
    Header.Custom("Accept", s"$GraphQLResponseJson, $Json;q=0.9")
  )

  // The zio-http client keeps only the last line of a repeated request header name.
  private def joinDuplicates(headers: List[Header]): Headers = {
    val joined = new java.util.LinkedHashMap[String, (String, java.lang.StringBuilder)]
    headers.foreach { header =>
      val key   = RemoteGraphQLConfig.lowercaseHeaderName(header.headerName)
      val known = joined.get(key)
      if (known eq null)
        joined.put(key, (RemoteGraphQLConfig.headerName(header), new java.lang.StringBuilder(header.renderedValue)))
      else known._2.append(if (key == "cookie") "; " else ", ").append(header.renderedValue)
    }
    val result = List.newBuilder[Header]
    joined.values.forEach { case (name, value) => result += Header.Custom(name, value.toString) }
    Headers.fromIterable(result.result())
  }

  private val layer: ZLayer[Any, Throwable, Client] = {
    val config = ZClient.Config.default.copy(
      connectionPool = ConnectionPoolConfig.Dynamic(minimum = 8, maximum = 256, ttl = 60.seconds),
      addUserAgentHeader = false,
      idleTimeout = None
    )
    (ZLayer.succeed(config) ++ ZLayer.succeed(
      NettyConfig.defaultWithFastShutdown
    ) ++ DnsResolver.system) >>> ZClient.live
  }

}
