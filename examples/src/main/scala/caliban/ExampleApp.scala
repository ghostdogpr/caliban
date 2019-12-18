package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.execution.QueryAnalyzer._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
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
import zio.interop.catz._
import zio.stream.ZStream

object ExampleApp extends CatsApp with GenericSchema[Console with Clock] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  case class Queries2(
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )

  case class Queries3(character: Int)

  type ExampleTask[A] = RIO[Console with Clock, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  def makeApi(service: ExampleService): GraphQL[Console with Clock] = {
    val api1 = graphQL(
      RootResolver(
        Queries(args => service.getCharacters(args.origin)),
        Mutations(args => service.deleteCharacter(args.name)),
        Subscriptions(service.deletedEvents)
      )
    )
    val api2 = graphQL(RootResolver(Queries2(args => service.findCharacter(args.name))))
    val api3 = graphQL(RootResolver(Queries3(2)))

    maxDepth(30)(
      maxFields(200)(
        (api1 |+| api2 |+| api3).rename(queriesName = Some("Queries"))
      )
    )
  }

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      blocker <- ZIO
                  .accessM[Blocking](_.blocking.blockingExecutor.map(_.asEC))
                  .map(Blocker.liftExecutionContext)
      service     <- ExampleService.make(sampleCharacters)
      interpreter = makeApi(service).interpreter
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
