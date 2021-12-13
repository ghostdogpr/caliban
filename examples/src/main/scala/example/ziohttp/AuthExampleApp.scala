package example.ziohttp

import caliban.GraphQL.graphQL
import caliban.Value.StringValue
import caliban._
import caliban.interop.tapir.{ StreamTransformer, WebSocketHooks }
import caliban.schema.GenericSchema
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import io.netty.handler.codec.http.{ HttpHeaderNames, HttpHeaderValues }
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.clock._
import zio.duration._
import zio.stream._

case object Unauthorized extends RuntimeException("Unauthorized")

trait Auth {
  type Unauthorized = Unauthorized.type

  def currentUser: IO[Unauthorized, String]
  def setUser(name: String): UIO[Unit]
}

object Auth {

  val http: ULayer[Has[Auth]] =
    FiberRef
      .make[String](null)
      .map { ref =>
        new Auth {
          def currentUser: IO[Unauthorized, String] =
            ref.get.flatMap(v => if (v eq null) ZIO.fail(Unauthorized) else ZIO.succeed(v))
          def setUser(name: String): UIO[Unit]      = ref.set(name)
        }
      }
      .toLayer

  object WebSockets {
    private val wsSession = Http.fromEffect(Ref.make[String](null))

    def live[R <: Has[Auth] with Clock](interpreter: GraphQLInterpreter[R, CalibanError]) =
      wsSession.flatMap { session =>
        val auth =
          new Auth {
            def currentUser: IO[Unauthorized, String] =
              session.get.flatMap(v => if (v eq null) ZIO.fail(Unauthorized) else ZIO.succeed(v))
            def setUser(name: String): UIO[Unit]      = session.set(name)
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
            .flatMap(auth.setUser)
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

  def middleware[R, B](app: Http[R, Throwable, Request, Response[R, Throwable]]): HttpApp[Has[Auth], Nothing] =
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
    whoAmI: ZIO[Has[Auth], Nothing, String] = ZIO.service[Auth].flatMap(_.currentUser)
  )
  case class Subscriptions(
    whoAmI: ZStream[Has[Auth] with Clock, Unauthorized.type, String] =
      ZStream.fromEffect(ZIO.service[Auth].flatMap(_.currentUser)).repeat(Schedule.spaced(10.seconds))
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
