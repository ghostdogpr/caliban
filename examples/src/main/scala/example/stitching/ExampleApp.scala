package example.stitching

import caliban._
import caliban.quick._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.schema._
import caliban.tools.stitching.{ HttpRequest, RemoteResolver, RemoteSchemaResolver, ResolveRequest }
import caliban.tools.{ Options, RemoteSchema, SchemaLoader }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
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

object ExampleApp extends ZIOAppDefault {
  def run =
    (StitchingExample.enrichedApi @@ Wrappers.printErrors).flatMap {
      _.runServer(
        port = 8080,
        apiPath = "/api/graphql",
        graphiqlPath = Some("/graphiql"),
        webSocketPath = Some("/ws/graphql")
      )
    }.provide(
      HttpClientZioBackend.layer(),
      Configuration.fromEnvironment
    )
}
