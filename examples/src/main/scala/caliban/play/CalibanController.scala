package caliban.play

import akka.actor.ActorSystem
import akka.stream.Materializer
import caliban.{ play => _, _ }
import caliban.ExampleData.sampleCharacters
import caliban.ExampleService.ExampleService
import play.api.mvc._
import zio.Runtime
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import scala.concurrent.ExecutionContextExecutor

class CalibanController(val controllerComponents: ControllerComponents)(
  implicit actorSystem: ActorSystem,
  materializer: Materializer
) extends BaseController {

  val calibanPlayAdapter: PlayAdapter =
    PlayAdapter(controllerComponents.parsers, controllerComponents.actionBuilder)

  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val runtime: Runtime[ExampleService with Console with Clock] =
    Runtime.unsafeFromLayer(ExampleService.make(sampleCharacters) ++ Console.live ++ Clock.live, Platform.default)

  val interpreter = runtime.unsafeRun(ExampleApi.api.interpreter)

  def graphqlGet(
    query: String,
    variables: Option[String],
    operation: Option[String],
    extensions: Option[String]
  ): Action[AnyContent] =
    calibanPlayAdapter.makeGetAction(interpreter)(query, variables, operation, extensions)

  def graphqlPost(): Action[GraphQLRequest] = calibanPlayAdapter.makePostAction(interpreter)

  def webSocket(): WebSocket =
    calibanPlayAdapter.makeWebSocket(interpreter)

}
