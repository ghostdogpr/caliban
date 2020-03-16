package caliban.http4s

import scala.language.postfixOps
import caliban.ExampleData._
import caliban.ExampleService.ExampleService
import caliban.GraphQL._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers._
import caliban.{ ExampleService, Http4sAdapter, RootResolver }
import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.console.putStrLn
import zio.duration._
import zio.interop.catz._
import zio.stream.ZStream

object ExampleApp extends CatsApp with GenericSchema[ExampleService] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[ExampleService, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[ExampleService, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[ExampleService, Boolean])
  case class Subscriptions(characterDeleted: ZStream[ExampleService, Nothing, String])

  type ExampleTask[A] = RIO[ZEnv, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  val api =
    graphQL(
      RootResolver(
        Queries(
          args => ExampleService.getCharacters(args.origin),
          args => ExampleService.findCharacter(args.name)
        ),
        Mutations(args => ExampleService.deleteCharacter(args.name)),
        Subscriptions(ExampleService.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ExampleService
      .make(sampleCharacters)
      .memoize
      .use(layer =>
        for {
          blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
          interpreter <- api.interpreter.map(_.provideCustomLayer(layer))
          _ <- BlazeServerBuilder[ExampleTask]
                .bindHttp(8088, "localhost")
                .withHttpApp(
                  Router[ExampleTask](
                    "/api/graphql" -> CORS(Http4sAdapter.makeHttpService(interpreter)),
                    "/ws/graphql"  -> CORS(Http4sAdapter.makeWebSocketService(interpreter)),
                    "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", blocker, None))
                  ).orNotFound
                )
                .resource
                .toManaged
                .useForever
        } yield 0
      )
      .catchAll(err => putStrLn(err.toString).as(1))
}
