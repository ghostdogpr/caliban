package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }

import caliban.ZHttpAdapter
import io.netty.handler.codec.http.{ HttpHeaderNames, HttpHeaderValues }
import zio._
import zio.stream._
import zhttp.http._
import zhttp.service.Server

object ExampleApp extends ZIOAppDefault {
  private val graphiql =
    Http.succeed(
      Response.http(
        content = HttpData.fromStream(ZStream.fromResource("graphiql.html")),
        headers = List(Header(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_HTML))
      )
    )

  override def run: ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route {
                           case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> Root / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
