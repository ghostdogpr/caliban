package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.schema.Annotations.{GQLDeprecated, GQLDescription}
import caliban.schema.GenericSchema
import cats.data.{Kleisli, OptionT}
import org.http4s.{EntityEncoder, HttpApp, HttpRoutes, Response, Status}
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.{Console, putStrLn}
import zio.interop.catz._
import zio.stream.ZStream

import scala.io.Source

object ExampleApp extends CatsApp with GenericSchema[Console with Clock] {

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  type ExampleTask[A] = RIO[Console with Clock with Blocking, A]

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  def staticResource(path: String): ExampleTask[Response[ExampleTask]] =
    blocking.effectBlocking(Source.fromResource(path).getLines().mkString("\n"))
      .map(content => Response[ExampleTask](Status.Ok, body = EntityEncoder[ExampleTask, String](EntityEncoder.stringEncoder).toEntity(content).body))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      service <- ExampleService.make(sampleCharacters)
      interpreter = graphQL(
        RootResolver(
          Queries(
            args => service.getCharacters(args.origin),
            args => service.findCharacter(args.name)
          ),
          Mutations(args => service.deleteCharacter(args.name)),
          Subscriptions(service.deletedEvents)
        )
      )
      _ <- BlazeServerBuilder[ExampleTask]
            .bindHttp(8088, "localhost")
            .withHttpApp(
              Router[ExampleTask](
                "/api/graphql" -> CORS(Http4sAdapter.makeRestService(interpreter)),
                "/ws/graphql"  -> CORS(Http4sAdapter.makeWebSocketService(interpreter)),
                "/graphiql" -> Kleisli.liftF(OptionT.liftF(staticResource("graphiql.html")))
              ).orNotFound
            )
            .resource
            .toManaged
            .useForever
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
