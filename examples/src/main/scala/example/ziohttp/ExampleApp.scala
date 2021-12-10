package example.ziohttp

import caliban.ZHttpAdapter
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import io.netty.handler.codec.http.{ HttpHeaderNames, HttpHeaderValues }
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.stream._

object ExampleApp extends App {
  private val graphiql =
    Http.succeed(
      Response(
        headers = List(Header(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_HTML)),
        data = HttpData.fromStream(ZStream.fromResource("graphiql.html"))
      )
    )

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route[Request] {
                           case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> !! / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
