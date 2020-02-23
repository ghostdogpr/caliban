package caliban

import caliban.PlayJson._
import javax.inject.Inject
import play.api.mvc.{ AbstractController, Action, ControllerComponents }
import zio.Runtime

class CalibanController[-R] @Inject() (cc: ControllerComponents, runtime: Runtime[R])
    extends AbstractController(cc) {

  def action[E](interpreter: GraphQLInterpreter[Any, E]): Action[GraphQLRequest] =
    Action.async(parse.json[GraphQLRequest]) { req =>
      runtime
        .unsafeRunToFuture(
          interpreter
            .execute(req.body.query, req.body.operationName, req.body.variables.getOrElse(Map.empty))
            .map(Ok(_))
        )
        .future
    }

}
