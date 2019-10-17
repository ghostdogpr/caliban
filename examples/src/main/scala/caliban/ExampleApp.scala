package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.interop.catz._
import zio.stream.ZStream
import zio.{ RIO, URIO, ZEnv, ZIO }

object ExampleApp extends CatsApp with GenericSchema[Console with Clock] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  type ExampleTask[A] = RIO[Console with Clock, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      service <- ExampleService.make(sampleCharacters)
      interpreter = graphQL(
        RootResolver(
          Queries(args => service.getCharacters(args.origin), args => service.findCharacter(args.name)),
          Mutations(args => service.deleteCharacter(args.name)),
          Subscriptions(service.deletedEvents)
        )
      )
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
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
