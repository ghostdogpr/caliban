package caliban

import caliban.PlayJson._
import javax.inject.Inject
import play.api.mvc.{ AbstractController, Action, ControllerComponents }
import zio.DefaultRuntime

abstract class CalibanController @Inject() (cc: ControllerComponents)
    extends AbstractController(cc)
    with DefaultRuntime {

  def action[E](interpreter: GraphQLInterpreter[Any, E]): Action[GraphQLRequest] =
    Action.async(parse.json[GraphQLRequest]) { req =>
      unsafeRunToFuture(
        interpreter.execute(req.body.query, req.body.operationName, req.body.variables.getOrElse(Map.empty)).map(Ok(_))
      ).future
    }

}
