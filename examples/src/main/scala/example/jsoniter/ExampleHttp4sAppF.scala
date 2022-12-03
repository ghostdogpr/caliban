package example.jsoniter

import caliban.Http4sAdapter.{ convertHttpEndpointToF, convertWebSocketEndpointToF }
import caliban.interop.cats.implicits._
import caliban.interop.cats.{ CatsInterop, ToEffect }
import caliban.interop.tapir.TapirAdapter
import caliban.{ CalibanError, GraphQLInterpreter }
import cats.data.Kleisli
import cats.effect.std.Dispatcher
import cats.effect.{ ExitCode, IO, IOApp }
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import sttp.tapir.server.http4s.Http4sServerInterpreter
import zio.{ Runtime, Unsafe }

/**
 * Notice: The jsoniter interop can be used only on JDK versions 11+
 *
 * If using jsoniter with a schema that contains recursive types, make sure to limit the maximum depth of queries using
 * the `maxDepth` wrapper to a value of 512 or less
 */
object ExampleHttp4sAppF extends IOApp {
  import sttp.tapir.json.jsoniter._ // Required

  type MyEnv = ExampleService

  implicit val zioRuntime: Runtime[MyEnv] =
    Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(ExampleService.make(sampleCharacters)))

  private def httpRoutesF(
    interpreter: GraphQLInterpreter[MyEnv, CalibanError]
  )(implicit interop: ToEffect[IO, MyEnv]) = {
    val endpoints  = TapirAdapter.makeHttpService[MyEnv, CalibanError](interpreter)
    val endpointsF = endpoints.map(convertHttpEndpointToF[IO, MyEnv])
    Http4sServerInterpreter[IO]().toRoutes(endpointsF)
  }

  private def wsRoutesF(
    builder: WebSocketBuilder2[IO],
    interpreter: GraphQLInterpreter[MyEnv, CalibanError]
  )(implicit interop: CatsInterop[IO, MyEnv]) = {
    val endpoint  = TapirAdapter.makeWebSocketService[MyEnv, CalibanError](interpreter)
    val endpointF = convertWebSocketEndpointToF[IO, MyEnv](endpoint)
    Http4sServerInterpreter[IO]().toWebSocketRoutes(endpointF)(builder)
  }

  // Needed to build upload routes
  private implicit val stringMapCodec: JsonValueCodec[Map[String, Seq[String]]] = JsonCodecMaker.make

  private def uploadRoutes(interpreter: GraphQLInterpreter[ExampleService, CalibanError]) = {
    val endpoint  = TapirAdapter.makeHttpUploadService[ExampleService, CalibanError](interpreter)
    val endpointF = convertHttpEndpointToF[IO, MyEnv](endpoint)
    Http4sServerInterpreter[IO]().toRoutes(endpointF)
  }

  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher.parallel[IO].use { implicit dispatcher =>
      for {
        interpreter <- ExampleApi.api.interpreterAsync[IO]
        _           <- BlazeServerBuilder[IO]
                         .bindHttp(8088, "localhost")
                         .withHttpWebSocketApp(wsBuilder =>
                           Router[IO](
                             "/api/graphql"    -> CORS.policy(httpRoutesF(interpreter)),
                             "/ws/graphql"     -> CORS.policy(wsRoutesF(wsBuilder, interpreter)),
                             "/upload/graphql" -> CORS.policy(uploadRoutes(interpreter)),
                             "/graphiql"       ->
                               Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                           ).orNotFound
                         )
                         .serve
                         .compile
                         .drain
      } yield ExitCode.Success
    }
}
