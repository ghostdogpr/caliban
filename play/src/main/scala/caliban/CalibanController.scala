package caliban

import caliban.PlayJson._
import caliban.Value.NullValue
import javax.inject.Inject
import play.api.mvc.{ AbstractController, Action, ControllerComponents }
import zio.{ Exit, Runtime, ZIO }

import scala.concurrent.{ Future, Promise }

class CalibanController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {

  private def unsafeRunToFuture[R, E <: Throwable, A](zio: ZIO[R, E, A])(runtime: Runtime[R]): Future[A] = {
    val p = Promise[A]()
    runtime.unsafeRunAsync(zio) {
      case Exit.Success(value) => p.success(value)
      case Exit.Failure(cause) => p.failure(cause.squashTrace)
    }
    p.future
  }

  def action[R, E](runtime: Runtime[R], interpreter: GraphQLInterpreter[R, E]): Action[GraphQLRequest] =
    Action.async(parse.json[GraphQLRequest]) { req =>
      unsafeRunToFuture(
        interpreter
          .execute(req.body.query, req.body.operationName, req.body.variables.getOrElse(Map.empty))
          .catchAllCause(cause => ZIO.succeed(GraphQLResponse(NullValue, cause.defects)))
          .map(Ok(_))
      )(runtime)
    }
}
