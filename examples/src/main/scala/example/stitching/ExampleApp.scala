package example.stitching

import zio._

import caliban._
import caliban.GraphQL.graphQL
import caliban.schema._
import caliban.tools.{ Options, RemoteSchema, SchemaLoader }
import caliban.tools.stitching.{ HttpRequest, RemoteResolver, RemoteSchemaResolver, ResolveRequest }

import sttp.client3.asynchttpclient.zio._

object StitchingExample extends GenericSchema[ZEnv] {
  val GITHUB_API = "https://api.github.com/graphql"

  case class User(login: String)
  case class AppUser(id: String, name: String, githubProfile: User)

  case class Queries(
    GetUser: String => URIO[ZEnv, AppUser]
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
    remoteSchemaResolvers = RemoteSchemaResolver.fromSchema(remoteSchema, GITHUB_API)
  } yield {
    val remoteResolvers = remoteSchemaResolvers.resolvers
    val apiRequest      =
      remoteResolvers.toQuery >>> remoteResolvers.request >>> RemoteResolver.fromEffect((r: HttpRequest) =>
        for {
          config <- ZIO.service[Configuration]
        } yield r.header("Authorization", s"Bearer ${config.githubToken}")
      ) >>> remoteResolvers.execute >>> remoteResolvers.unwrap

    implicit val githubProfileSchema: Schema[ZEnv, User] =
      remoteSchemaResolvers
        .remoteResolver("User")(
          RemoteResolver.fromFunction((r: ResolveRequest[User]) =>
            r.field.copy(name = "user", arguments = Map("login" -> caliban.Value.StringValue(r.args.login)))
          ) >>> apiRequest
        )
        .provide(sttpClient ++ config)

    graphQL(
      RootResolver(
        Queries(
          GetUser =
            name => random.nextUUID.map(uuid => AppUser(id = uuid.toString(), name = name, githubProfile = User(name)))
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
