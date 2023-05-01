package example.ziohttp

import caliban.Value.StringValue
import caliban._
import caliban.interop.tapir.{ HttpInterpreter, WebSocketHooks }
import caliban.schema.GenericSchema
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import sttp.tapir.json.circe._
import zio._
import zio.http._
import zio.stream._

case object Unauthorized extends RuntimeException("Unauthorized")

trait Auth {
  type Unauthorized = Unauthorized.type

  def currentUser: IO[Unauthorized, String]
  def setUser(name: Option[String]): UIO[Unit]
}

object Auth {

  val http: ULayer[Auth] = ZLayer.scoped {
    FiberRef
      .make[Option[String]](None)
      .map { ref =>
        new Auth {
          def currentUser: IO[Unauthorized, String]    =
            ref.get.flatMap {
              case Some(v) => ZIO.succeed(v)
              case None    => ZIO.fail(Unauthorized)
            }
          def setUser(name: Option[String]): UIO[Unit] = ref.set(name)
        }
      }
  }

  object WebSockets {
    def live[R <: Auth](
      interpreter: GraphQLInterpreter[R, CalibanError]
    ): App[R] = {
      val webSocketHooks = WebSocketHooks.init[R, CalibanError](payload =>
        ZIO
          .fromOption(payload match {
            case InputValue.ObjectValue(fields) =>
              fields.get("Authorization").flatMap {
                case StringValue(s) => Some(s)
                case _              => None
              }
            case x                              => None
          })
          .orElseFail(CalibanError.ExecutionError("Unable to decode payload"))
          .flatMap(user => ZIO.serviceWithZIO[Auth](_.setUser(Some(user))))
          .debug("connect")
      ) ++ WebSocketHooks.afterInit(ZIO.failCause(Cause.empty).delay(10.seconds))

      ZHttpAdapter.makeWebSocketService(interpreter, webSocketHooks = webSocketHooks)
    }
  }

  def middleware[R] = HttpAppMiddleware.customAuthZIO { headers =>
    val user = headers.get(Header.Authorization).map(_.renderedValue)
    ZIO.serviceWithZIO[Auth](_.setUser(user)).as(true)
  }
}

object Authed extends GenericSchema[Auth] {
  import auto._

  case class Queries(
    whoAmI: ZIO[Auth, Unauthorized.type, String] = ZIO.serviceWithZIO[Auth](_.currentUser)
  )
  case class Subscriptions(
    whoAmI: ZStream[Auth, Unauthorized.type, String] =
      ZStream.fromZIO(ZIO.serviceWithZIO[Auth](_.currentUser)).repeat(Schedule.spaced(2.seconds))
  )

  val api = graphQL(RootResolver(Queries(), None, Subscriptions()))
}

object AuthExampleApp extends ZIOAppDefault {
  private val graphiql = Handler.fromStream(ZStream.fromResource("graphiql.html")).toHttp

  override def run =
    (for {
      interpreter <- (ExampleApi.api |+| Authed.api).interpreter
      port        <- Server
                       .install(
                         Http
                           .collectHttp[Request] {
                             case _ -> !! / "api" / "graphql" =>
                               ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter)) @@ Auth.middleware
                             case _ -> !! / "ws" / "graphql"  => Auth.WebSockets.live(interpreter)
                             case _ -> !! / "graphiql"        => graphiql
                           }
                           .withDefaultErrorResponse
                       )
      _           <- ZIO.logInfo(s"Server started on port $port")
      _           <- ZIO.never
    } yield ())
      .provide(ExampleService.make(sampleCharacters), Auth.http, Server.default)
      .exitCode
}
