package caliban.play

import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printSlowQueries, timeout }
import caliban.{ ExampleService, GraphQL, PlayCaliban, RootResolver }
import play.api.{ ApplicationLoader, BuiltInComponentsFromContext, Environment, NoHttpFiltersComponents }
import play.api.mvc.DefaultControllerComponents
import play.api.routing.Router
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.stream.ZStream
import zio.{ Runtime, URIO }

import scala.io.StdIn.readLine
import scala.language.postfixOps

object ExampleApp extends GenericSchema[Console with Clock] {

  implicit val runtime = Runtime.unsafeFromLayer(Console.live ++ Clock.live)

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  def makeApi(service: ExampleService): GraphQL[Console with Clock] =
    graphQL(
      RootResolver(
        Queries(
          args => service.getCharacters(args.origin),
          args => service.findCharacter(args.name)
        ),
        Mutations(args => service.deleteCharacter(args.name)),
        Subscriptions(service.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(5 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

  val service     = runtime.unsafeRun(ExampleService.make(sampleCharacters))
  val interpreter = runtime.unsafeRun(makeApi(service).interpreter)

  def main(args: Array[String]): Unit = {

    val context = ApplicationLoader.Context.create(Environment.simple())
    val components = new BuiltInComponentsFromContext(context) with NoHttpFiltersComponents {
      val playCaliban = new PlayCaliban(runtime)
      val controller = new playCaliban.Controller(
        DefaultControllerComponents(
          defaultActionBuilder,
          playBodyParsers,
          messagesApi,
          langs,
          fileMimeTypes,
          executionContext
        )
      )

      override def router: Router = Router.from {
        case POST(p"/api/graphql") => controller.action(interpreter)
      }
    }

    val server = AkkaHttpServer.fromApplication(
      components.application,
      ServerConfig(
        port = Some(8088),
        address = "127.0.0.1"
      )
    )

    println("Server started, press enter to exit ...")
    readLine()
    server.stop()
  }

}
