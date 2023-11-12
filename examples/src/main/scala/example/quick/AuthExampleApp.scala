package example.quick

import caliban._
import caliban.schema.GenericSchema
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
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

  val middleware =
    HttpAppMiddleware.customAuthZIO { req =>
      val user = req.headers.get(Header.Authorization).map(_.renderedValue)
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

  override def run: URIO[Any, ExitCode] =
    (for {
      exampleApi   <- ZIO.service[GraphQL[Any]]
      interpreter  <- (exampleApi |+| Authed.api).interpreter
      apiRoute      = QuickAdapter(interpreter).handler.toHttp @@ Auth.middleware
      graphiqlRoute = GraphiQLAdapter.handler(Root / "api" / "graphql", Root / "graphiql")
      port         <- Server.install(
                        Http.collectHttp[Request] {
                          case _ -> Root / "api" / "graphql" => apiRoute
                          case _ -> Root / "graphiql"        => graphiqlRoute.toHttp
                        }
                      )
      _            <- ZIO.logInfo(s"Server started on port $port")
      _            <- ZIO.never
    } yield ())
      .provide(
        ExampleService.make(sampleCharacters),
        ExampleApi.layer,
        Auth.http,
        Server.defaultWithPort(8090)
      )
      .exitCode
}
