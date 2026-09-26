package caliban.gateway.internal

import caliban.gateway.RemoteGraphQLConfig
import caliban.gateway.internal.RemoteTransport.{ BoundedBody, GraphQLResponseJson, Json }
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
    client(withHeaders(request, headers))

  def socket(url: URL, headers: List[Header], app: WebSocketApp[Any])(implicit
    trace: Trace
  ): ZIO[Scope, Throwable, Response] =
    client.url(url).addHeaders(joinDuplicates(headers)).socket(app)

  private def send(request: Request, headers: List[Header], maxResponseBytes: Int)(implicit trace: Trace): Task[Reply] =
    ZIO.scoped {
      client(withHeaders(request, headers)).flatMap { response =>
        response.header(Header.ContentLength).map(_.length) match {
          case Some(length) if length > maxResponseBytes =>
            ZIO.succeed(Reply(response, BoundedBody(Array.empty, limitExceeded = true)))
          case Some(_)                                   =>
            response.body.asArray.map(bytes => Reply(response, BoundedBody(bytes, limitExceeded = false)))
          case None                                      =>
            response.body.asStream
              .take(maxResponseBytes.toLong + 1L)
              .runCollect
              .map(bytes => Reply(response, BoundedBody(bytes.toArray, bytes.length > maxResponseBytes)))
        }
      }
    }
}

private[gateway] object GatewayHttpClient {
  def make(implicit trace: Trace): ZIO[Scope, Throwable, GatewayHttpClient] =
    layer.build.map(env => new GatewayHttpClient(env.get[Client]))

  final case class Reply(response: Response, body: BoundedBody) {
    def status: Status              = response.status
    def contentType: Option[String] = response.rawHeader(Header.ContentType)
  }

  val jsonContentType =
    Header.ContentType(MediaType.application.json, charset = Some(StandardCharsets.UTF_8))

  private val jsonHeaders = Headers(
    jsonContentType,
    Header.Custom("Accept", s"$GraphQLResponseJson, $Json;q=0.9")
  )

  private def withHeaders(request: Request, headers: List[Header]): Request =
    if (headers.isEmpty) request else request.addHeaders(joinDuplicates(headers))

  // The zio-http client keeps only the last line of a repeated request header name.
  private def joinDuplicates(headers: List[Header]): Headers =
    headers match {
      case Nil | _ :: Nil => Headers.fromIterable(headers)
      case _              =>
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
