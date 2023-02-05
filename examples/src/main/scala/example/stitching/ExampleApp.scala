package example.stitching

import caliban._
import caliban.GraphQL.graphQL
import caliban.schema._
import caliban.schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.tools.{ Options, RemoteSchema, SchemaLoader }
import caliban.tools.stitching.{ HttpRequest, RemoteResolver, RemoteSchemaResolver, ResolveRequest }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.zio._
import zio._

object StitchingExample extends GenericSchema[Any] {
  val GITHUB_API = "https://api.github.com/graphql"

  case class AppUser(id: String, name: String, featuredRepository: Repository)
  case class Repository(owner: String, name: String)

  case class GetUserQuery(name: String, repository: String)

  case class Queries(
    GetUser: GetUserQuery => URIO[Any, AppUser]
  )

  val api =
    for {
      config     <- ZIO.environment[Configuration]
      sttpClient <- ZIO.environment[SttpBackend[Task, ZioStreams with WebSockets]]

      schemaLoader = SchemaLoader.fromIntrospection(
                       GITHUB_API,
                       Some(
                         List(
                           Options.Header(
                             "Authorization",
                             s"Bearer ${config.get.githubToken}"
                           )
                         )
                       )
                     )

      schema               <- schemaLoader.load
      remoteSchema         <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(schema))
      remoteSchemaResolvers = RemoteSchemaResolver.fromSchema(remoteSchema)
    } yield {
      val apiRequest =
        RemoteResolver.toQuery >>> RemoteResolver.request(GITHUB_API) >>> RemoteResolver.fromFunctionM(
          (r: HttpRequest) =>
            for {
              config <- ZIO.service[Configuration]
            } yield r.header("Authorization", s"Bearer ${config.githubToken}")
        ) >>> RemoteResolver.execute >>> RemoteResolver.unwrap

      implicit val githubProfileSchema: Schema[Any, Repository] =
        remoteSchemaResolvers
          .remoteResolver("Repository")(
            RemoteResolver.fromFunction((r: ResolveRequest[Repository]) =>
              r.field.copy(
                name = "repository",
                arguments = Map(
                  "owner" -> Value.StringValue(r.args.owner),
                  "name"  -> Value.StringValue(r.args.name)
                )
              )
            ) >>> apiRequest
          )
          .provideEnvironment(sttpClient ++ config)

      graphQL(
        RootResolver(
          Queries(
            GetUser = query =>
              Random.nextUUID.map(uuid =>
                AppUser(
                  id = uuid.toString,
                  name = query.name,
                  featuredRepository = Repository(query.name, query.repository)
                )
              )
          )
        )
      )
    }
}

case class Configuration(githubToken: String)

object Configuration {
  def fromEnvironment = ZLayer {
    for {
      githubToken <- read("GITHUB_TOKEN")
    } yield Configuration(githubToken)
  }

  private def read(key: String): Task[String] = ZIO.attempt(sys.env(key))
}

import zio.stream._
import zhttp.http._
import zhttp.service.Server
import caliban.ZHttpAdapter

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Http.fromStream(ZStream.fromResource("graphiql.html"))

  override def run =
    (for {
      api         <- StitchingExample.api
      interpreter <- api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.collectHttp[Request] {
                           case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> !! / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideSomeLayer[Scope](
        AsyncHttpClientZioBackend.layer() ++ Configuration.fromEnvironment
      )
      .exitCode
}
