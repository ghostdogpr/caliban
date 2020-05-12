package caliban.play

import akka.actor.ActorSystem
import akka.stream.Materializer
import caliban.{play => _, _}
import caliban.ExampleService.ExampleService
import play.api.mvc._
import zio.{ Runtime, ZEnv, ZLayer }

class CalibanController(val controllerComponents: ControllerComponents)(
  implicit actorSystem: ActorSystem,
  materializer: Materializer
) extends BaseController {

  val calibanPlayAdapter: PlayAdapter =
    PlayAdapter(controllerComponents.parsers, controllerComponents.actionBuilder)

  implicit val runtime = Runtime.default
  implicit val ec      = actorSystem.dispatcher

  val service: ZLayer[Any, Nothing, ExampleService] = ExampleService.make(ExampleData.sampleCharacters)

  val interpreter: GraphQLInterpreter[ZEnv, CalibanError] = runtime.unsafeRun(
    ExampleService
      .make(ExampleData.sampleCharacters)
      .memoize
      .use(layer => ExampleApi.api.interpreter.map(_.provideCustomLayer(layer)))
  )

  def graphqlGet(
    query: String,
    variables: Option[String],
    operation: Option[String],
    extensions: Option[String]
  ): Action[AnyContent] =
    calibanPlayAdapter.makeGetAction(interpreter)(query, variables, operation, extensions)

  def graphqlPost(): Action[GraphQLRequest] = calibanPlayAdapter.makePostAction(interpreter)

  def webSocket(): WebSocket = calibanPlayAdapter.makeWebSocket(interpreter)

}
