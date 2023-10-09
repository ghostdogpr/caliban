package example.stitching

import caliban._
import caliban.gateway.{ SubGraph, SuperGraph }
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import caliban.introspection.adt.TypeVisitor
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.schema._
import caliban.wrappers.Wrappers
import sttp.client3.httpclient.zio._
import zio._

object StitchingExample extends GenericSchema[Any] {
  val GITHUB_API = "https://api.github.com/graphql"

  case class AppUser(id: String, name: String, repository: String)
  case class GetUserQuery(name: String, repository: String)
  case class Queries(GetUser: GetUserQuery => URIO[Any, AppUser])

  val api =
    graphQL(
      RootResolver(
        Queries(
          GetUser = query =>
            Random.nextUUID.map(uuid =>
              AppUser(
                id = uuid.toString,
                name = query.name,
                repository = query.repository
              )
            )
        )
      )
    )

  val enrichedApi =
    for {
      config     <- ZIO.service[Configuration]
      headers     = Map("Authorization" -> s"Bearer ${config.githubToken}")
      github      = SubGraph.graphQL("Github", GITHUB_API, headers, exposeAtRoot = false)
      caliban     = SubGraph.caliban("Caliban", api)
      superGraph <- SuperGraph
                      .compose(List(github, caliban))
                      .transform(TypeVisitor.renameField { case ("Queries", "GetUser") => "user" })
                      .transform(TypeVisitor.renameArgument { case ("Queries", "user") => ("repository", "repo") })
                      .extend(
                        github,
                        sourceFieldName = "repository",
                        targetTypeName = "AppUser",
                        targetFieldName = "featuredRepository",
                        argumentMappings = Map("repository" -> ("name" -> _), "name" -> ("owner" -> _))
                      )
                      // restrict exposed remote fields
                      .transform(TypeVisitor.filterField {
                        case ("Repository", "name")    => true
                        case ("Repository", _)         => false
                        case ("AppUser", "repository") => false
                        case ("AppUser", _)            => true
                      })
                      // remove interfaces that Repository extends
                      .transform(TypeVisitor.filterInterface { case ("Repository", _) => false })
                      .build
    } yield superGraph
}

case class Configuration(githubToken: String)

object Configuration {
  def fromEnvironment: ZLayer[Any, Throwable, Configuration] =
    ZLayer {
      for {
        githubToken <- System.env("GITHUB_TOKEN").orDie.someOrFailException
      } yield Configuration(githubToken)
    }
}

import caliban.ZHttpAdapter
import zio.http._
import zio.stream._

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Handler.fromStream(ZStream.fromResource("graphiql.html")).toHttp.withDefaultErrorResponse

  def run =
    (for {
      _           <- ZIO.debug("Building schema")
      api         <- StitchingExample.enrichedApi
      interpreter <- (api @@ Wrappers.printErrors).interpreter
      _           <- ZIO.debug("Starting server")
      _           <-
        Server
          .serve(
            Http
              .collectHttp[Request] {
                case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
                case _ -> Root / "ws" / "graphql"  =>
                  ZHttpAdapter.makeWebSocketService(WebSocketInterpreter(interpreter))
                case _ -> Root / "graphiql"        => graphiql
              }
          )
    } yield ())
      .provide(
        HttpClientZioBackend.layer(),
        Configuration.fromEnvironment,
        Server.default
      )
      .onExit(ZIO.debug(_))
}
