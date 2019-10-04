package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio.console.putStrLn
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.stream.ZStream
import zio.{ Task, UIO, ZIO }

object ExampleApp extends CatsApp {

  case class Queries(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => UIO[Boolean])
  case class Subscriptions(characterDeleted: ZStream[Any, Nothing, String])

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    (for {
      service <- ExampleService.make(sampleCharacters)
      interpreter = graphQL(
        RootResolver(
          Queries(args => service.getCharacters(args.origin), args => service.findCharacter(args.name)),
          Mutations(args => service.deleteCharacter(args.name)),
          Subscriptions(service.deletedEvents)
        )
      )
      _ <- BlazeServerBuilder[Task]
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
