package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.execution.QueryAnalyzer
import caliban.execution.QueryAnalyzer._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.interop.catz._
import zio.stream.ZStream

case class ContextData(cost: Int)

trait Context {
  def context: Ref[ContextData]
}

object ExampleApp extends CatsApp with GenericSchema[Console with Clock with Context] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]],
    cost: ZIO[Context, Nothing, Int]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  type ExampleTask[A] = RIO[Console with Clock with Context, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  def makeInterpreter(
    service: ExampleService
  ): GraphQL[Console with Clock with Context, Queries, Mutations, Subscriptions, CalibanError] =
    maxDepth(30)(
      maxFields(200)(
        graphQL(
          RootResolver(
            Queries(
              args => service.getCharacters(args.origin),
              args => service.findCharacter(args.name),
              ZIO.accessM[Context](_.context.get.map(_.cost))
            ),
            Mutations(args => service.deleteCharacter(args.name)),
            Subscriptions(service.deletedEvents)
          )
        )
      )
    ).withQueryAnalyzer { root =>
      val cost = QueryAnalyzer.countFields(root)
      ZIO.accessM[Context](_.context.update(_.copy(cost = cost))).as(root)
    }

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    Ref
      .make[ContextData](ContextData(0))
      .flatMap { contextRef =>
        ZIO
          .runtime[Console with Clock with Context]
          .flatMap { implicit runtime =>
            for {
              service     <- ExampleService.make(sampleCharacters)
              interpreter = makeInterpreter(service)
              _ <- BlazeServerBuilder[ExampleTask]
                    .bindHttp(8088, "localhost")
                    .withHttpApp(
                      Router(
                        "/api/graphql" -> CORS(Http4sAdapter.makeRestService(interpreter)),
                        "/ws/graphql"  -> CORS(Http4sAdapter.makeWebSocketService(interpreter))
                      ).orNotFound
                    )
                    .resource
                    .toManaged
                    .useForever
            } yield 0
          }
          .catchAll(err => putStrLn(err.toString).as(1))
          .provideSome[Console with Clock](
            env =>
              new Context with Console with Clock {
                override def context: Ref[ContextData]     = contextRef
                override val console: Console.Service[Any] = env.console
                override val clock: Clock.Service[Any]     = env.clock
              }
          )
      }
}
