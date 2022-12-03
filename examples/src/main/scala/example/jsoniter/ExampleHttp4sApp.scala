package example.jsoniter

import caliban.interop.tapir.TapirAdapter
import caliban.{ CalibanError, GraphQLInterpreter }
import cats.data.Kleisli
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import example.ExampleData._
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio._
import zio.interop.catz._

/**
 * Notice: The jsoniter interop can be used only on JDK versions 11+.
 *
 * If using jsoniter with a schema that contains recursive types, make sure to limit the maximum depth of queries using
 * the `maxDepth` wrapper to a value of 512 or less
 */
object ExampleHttp4sApp extends ZIOAppDefault {
  import sttp.tapir.json.jsoniter._ // Required

  type ExampleTask[A] = RIO[ExampleService, A]

  private def httpRoutes(interpreter: GraphQLInterpreter[ExampleService, CalibanError]) = {
    val endpoints = TapirAdapter.makeHttpService[ExampleService, CalibanError](interpreter)
    ZHttp4sServerInterpreter().from(endpoints).toRoutes
  }

  private def wsRoutes(
    builder: WebSocketBuilder2[ExampleTask],
    interpreter: GraphQLInterpreter[ExampleService, CalibanError]
  ) = {
    val endpoint = TapirAdapter.makeWebSocketService[ExampleService, CalibanError](interpreter)
    ZHttp4sServerInterpreter[ExampleService]()
      .fromWebSocket(endpoint)
      .toRoutes(builder)
  }

  // Needed to build upload routes
  private implicit val stringMapCodec: JsonValueCodec[Map[String, Seq[String]]] = JsonCodecMaker.make

  private def uploadRoutes(interpreter: GraphQLInterpreter[ExampleService, CalibanError]) = {
    val endpoint = TapirAdapter.makeHttpUploadService[ExampleService, CalibanError](interpreter)
    ZHttp4sServerInterpreter().from(endpoint).toRoutes
  }

  override def run =
    ZIO
      .runtime[ExampleService]
      .flatMap(implicit runtime =>
        for {
          interpreter <- ExampleApi.api.interpreter
          _           <- BlazeServerBuilder[ExampleTask]
                           .bindHttp(8088, "localhost")
                           .withHttpWebSocketApp(wsBuilder =>
                             Router[ExampleTask](
                               "/api/graphql"    -> CORS.policy(httpRoutes(interpreter)),
                               "/ws/graphql"     -> CORS.policy(wsRoutes(wsBuilder, interpreter)),
                               "/upload/graphql" -> CORS.policy(uploadRoutes(interpreter)),
                               "/graphiql"       -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                             ).orNotFound
                           )
                           .resource
                           .toScopedZIO *> ZIO.never
        } yield ()
      )
      .provideSomeLayer[Scope](ExampleService.make(sampleCharacters))
}
