package caliban.http4s

import scala.language.postfixOps
import caliban.ExampleData._
import caliban.GraphQL._
import caliban.execution.QueryAnalyzer._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }
import caliban.schema.GenericSchema
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers._
import caliban.{ ExampleService, GraphQL, Http4sAdapter, RootResolver }
import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.duration._
import zio.interop.catz._
import zio.stream.ZStream

object ExampleApp extends CatsApp with GenericSchema[Console with Clock] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]],
    test: Foo[Int]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  @GQLName("FooAnnotated")
  case class Foo[E](e: E)

  type ExampleTask[A] = RIO[Console with Clock, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  def makeApi(service: ExampleService): UIO[GraphQL[Console with Clock]] =
    apolloTracing(
      logSlowQueries(500 millis)( // wrapper that logs slow queries
        timeout(3 seconds)(       // wrapper that fails slow queries
          maxDepth(30)(           // query analyzer that limit query depth
            maxFields(200)(       // query analyzer that limit query fields
              graphQL(
                RootResolver(
                  Queries(
                    args => service.getCharacters(args.origin),
                    args => service.findCharacter(args.name),
                    Foo(2)
                  ),
                  Mutations(args => service.deleteCharacter(args.name)),
                  Subscriptions(service.deletedEvents)
                )
              )
            )
          )
        )
      )
    )

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      blocker <- ZIO
                  .accessM[Blocking](_.blocking.blockingExecutor.map(_.asEC))
                  .map(Blocker.liftExecutionContext)
      service     <- ExampleService.make(sampleCharacters)
      api         <- makeApi(service)
      interpreter = api.interpreter
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
    } yield 0)
      .catchAll(err => putStrLn(err.toString).as(1))
}
