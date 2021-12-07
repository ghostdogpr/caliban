package example.ziohttp

import caliban.GraphQL.graphQL
import caliban.Value.StringValue
import caliban._
import caliban.interop.tapir.{ StreamTransformer, WebSocketHooks }
import caliban.schema.GenericSchema
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import io.netty.handler.codec.http.{HttpHeaderNames, HttpHeaderValues}
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.clock._
import zio.duration._
import zio.stream._

trait Auth {
  def currentUser: ZIO[Any, Throwable, String]
  def setUser(name: String): ZIO[Any, Throwable, Any]
}

object Auth {
  val http = FiberRef
    .make("unknown")
    .map { ref =>
      new Auth {
        def currentUser: ZIO[Any, Throwable, String]        = ref.get
        def setUser(name: String): ZIO[Any, Throwable, Any] = ref.set(name)
      }
    }
    .toLayer

  object WebSockets {
    val wsSession = Http.fromEffect(Ref.make[String]("unknown"))

    def live[R <: Has[Auth] with Clock](interpreter: GraphQLInterpreter[R, CalibanError]) =
      wsSession.flatMap { session =>
        val auth = new Auth {
          def currentUser: ZIO[Any, Throwable, String]        = session.get
          def setUser(name: String): ZIO[Any, Throwable, Any] = session.set(name)
        }

        val webSocketHooks = WebSocketHooks.init[R, CalibanError](payload =>
          ZIO
            .fromOption(payload match {
              case InputValue.ObjectValue(fields) =>
                fields.get("Authorization").flatMap {
                  case StringValue(s) => Some(s)
                  case _              => None
                }
              case _                              => None
            })
            .orElseFail(CalibanError.ExecutionError("Unable to decode payload"))
            .flatMap(user => ZIO.service[Auth].flatMap(_.setUser(user).orDie))
        ) ++
          WebSocketHooks.afterInit(ZIO.halt(Cause.empty).delay(10.seconds)) ++
          WebSocketHooks
            .message(new StreamTransformer[Has[Auth], Nothing] {
              def transform[R1 <: Has[Auth], E1 >: Nothing](
                stream: ZStream[R1, E1, GraphQLWSOutput]
              ): ZStream[R1, E1, GraphQLWSOutput] = stream.updateService[Auth](_ => auth)
            })

        ZHttpAdapter
          .makeWebSocketService(interpreter, webSocketHooks = webSocketHooks)
      }
  }

  def middleware[R, B](app: Http[R, Throwable, Request, Response[R, Throwable]]) =
    Http
      .fromEffectFunction[Request] { (request: Request) =>
        val user = request.headers
          .find(_.name == "Authorization")
          .map(_.value.toString())
          .getOrElse("unknown")

        ZIO
          .service[Auth]
          .flatMap(_.setUser(user))
          .fold(_ => Http.fail(CalibanError.ExecutionError("Failed to decode user")), _ => app)
      }
      .flatten
}

object Authed extends GenericSchema[ZEnv with Has[Auth]] {
  case class Queries(
    whoAmI: ZIO[Has[Auth], Nothing, String] = ZIO.service[Auth].flatMap(_.currentUser.orDie)
  )
  case class Subscriptions(
    whoAmI: ZStream[Has[Auth] with Clock, Nothing, String] =
      ZStream.fromEffect(ZIO.service[Auth].flatMap(_.currentUser.orDie)).repeat(Schedule.spaced(10.seconds))
  )

  val api = graphQL(RootResolver(Queries(), None, Subscriptions()))
}

object AuthExampleApp extends App {
  private val graphiql =
    Http.succeed(
      Response.http(
        content = HttpData.fromStream(ZStream.fromResource("graphiql.html")),
        headers = List(Header(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_HTML))
      )
    )
  
  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- (ExampleApi.api |+| Authed.api).interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route {
                           case _ -> Root / "api" / "graphql" => Auth.middleware(ZHttpAdapter.makeHttpService(interpreter))
                           case _ -> Root / "ws" / "graphql"  => Auth.WebSockets.live(interpreter)
                           case _ -> Root / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters) ++ Auth.http)
      .exitCode
}
