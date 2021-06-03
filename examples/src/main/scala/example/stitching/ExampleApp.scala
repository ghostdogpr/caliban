package example.stitching

import caliban._
import caliban.GraphQL.graphQL
import caliban.schema._
import caliban.tools.{ Options, RemoteSchema, SchemaLoader }
import caliban.tools.stitching.{ HttpRequest, RemoteResolver, RemoteSchemaResolver, ResolveRequest }

import sttp.client3.asynchttpclient.zio._
import zio._

object StitchingExample extends GenericSchema[ZEnv] {
  val GITHUB_API = "https://api.github.com/graphql"

  case class AppUser(id: String, name: String, featuredRepository: Repository)
  case class Repository(owner: String, name: String)

  case class GetUserQuery(name: String, repository: String)

  case class Queries(
    GetUser: GetUserQuery => URIO[ZEnv, AppUser]
  )

  val api = for {
    config     <- ZIO.environment[Has[Configuration]]
    sttpClient <- ZIO.environment[SttpClient]

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
      RemoteResolver.toQuery >>> RemoteResolver.request(GITHUB_API) >>> RemoteResolver.fromFunctionM((r: HttpRequest) =>
        for {
          config <- ZIO.service[Configuration]
        } yield r.header("Authorization", s"Bearer ${config.githubToken}")
      ) >>> RemoteResolver.execute >>> RemoteResolver.unwrap

    implicit val githubProfileSchema: Schema[ZEnv, Repository] =
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
        .provide(sttpClient ++ config)

    graphQL(
      RootResolver(
        Queries(
          GetUser = query =>
            random.nextUUID.map(uuid =>
              AppUser(
                id = uuid.toString(),
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
  def fromEnvironment =
    (for {
      githubToken <- read("GITHUB_TOKEN")
    } yield Configuration(githubToken)).toLayer

  private def read(key: String): Task[String] = Task.effect(
    sys.env(key)
  )
}

import zio._
import zio.stream._
import zhttp.http._
import zhttp.service.Server
import caliban.ZHttpAdapter

object ExampleApp extends App {
  private val graphiql =
    Http.succeed(Response.http(content = HttpData.fromStream(ZStream.fromResource("graphiql.html"))))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      api         <- StitchingExample.api
      interpreter <- api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route {
                           case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> Root / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideCustomLayer(
        AsyncHttpClientZioBackend.layer() ++ Configuration.fromEnvironment
      )
      .exitCode
}
