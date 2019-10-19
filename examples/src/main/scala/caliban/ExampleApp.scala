package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.{ DataSource, Fetch, GenericSchema }
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.interop.catz._
import zio.stream.ZStream

object ExampleApp extends CatsApp with GenericSchema[Console with Clock] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]],
    fetchCharacter: CharacterArgs => Fetch[Character]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  type ExampleTask[A] = RIO[Console with Clock, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  def source(service: ExampleService): DataSource[String, Character] =
    new DataSource[String, Character] {
      override def name                                      = "Characters"
      override def fetch(id: String): UIO[Option[Character]] = service.findCharacter(id)
      override def batch(ids: List[String]): UIO[Map[String, Character]] =
        service
          .getCharacters(None)
          .map(_.filter(c => ids.contains(c.name)).groupBy(_.name).map { case (k, v) => k -> v.head })
    }

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      service <- ExampleService.make(sampleCharacters)
      interpreter = graphQL(
        RootResolver(
          Queries(
            args => service.getCharacters(args.origin),
            args => service.findCharacter(args.name),
            args => Fetch(args.name, source(service))
          ),
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
