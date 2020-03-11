package caliban

import caliban.PlayJson._
import caliban.Value.NullValue
import javax.inject.Inject
import play.api.mvc.{ AbstractController, Action, ControllerComponents }
import zio.{ Runtime, ZIO }

class PlayCaliban[R](runtime: Runtime[R]) {

  class Controller @Inject() (cc: ControllerComponents) extends AbstractController(cc) {

    def action[E](interpreter: GraphQLInterpreter[R, E]): Action[GraphQLRequest] =
      Action.async(parse.json[GraphQLRequest]) { req =>
        runtime
          .unsafeRunToFuture(
            interpreter
              .execute(req.body.query, req.body.operationName, req.body.variables.getOrElse(Map.empty))
              .catchAllCause(cause => ZIO.succeed(GraphQLResponse(NullValue, cause.defects)))
              .map(Ok(_))
          )
          .future
      }
  }
}
