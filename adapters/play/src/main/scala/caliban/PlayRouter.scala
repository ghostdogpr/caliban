package caliban

import scala.concurrent.ExecutionContext
import akka.stream.Materializer
import caliban.PlayAdapter.RequestWrapper
import caliban.execution.QueryExecution
import play.api.mvc._
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._
import zio.Runtime
import zio.blocking.Blocking
import zio.duration.Duration
import zio.random.Random

case class PlayRouter[R <: Blocking with Random, E](
  interpreter: GraphQLInterpreter[R, E],
  controllerComponents: ControllerComponents,
  playground: Boolean = true,
  allowGETRequests: Boolean = true,
  subscriptions: Boolean = true,
  skipValidation: Boolean = false,
  enableIntrospection: Boolean = true,
  keepAliveTime: Option[Duration] = None,
  requestWrapper: RequestWrapper[R] = RequestWrapper.empty,
  queryExecution: QueryExecution = QueryExecution.Parallel
)(implicit runtime: Runtime[R], materializer: Materializer)
    extends SimpleRouter
    with PlayAdapter[R] {

  override val actionBuilder: ActionBuilder[Request, AnyContent] = controllerComponents.actionBuilder
  override val parse: PlayBodyParsers                            = controllerComponents.parsers
  implicit val ec: ExecutionContext                              = controllerComponents.executionContext

  override def routes: Routes = {
    case POST(
          p"/api/graphql" ? q_o"query=$query" & q_o"variables=$variables" & q_o"operationName=$operation" & q_o"extensions=$extensions"
        ) =>
      query match {
        case Some(_) =>
          makeGetAction(interpreter, skipValidation, enableIntrospection, queryExecution)(
            query,
            variables,
            operation,
            extensions
          )
        case None    => makePostAction(interpreter, skipValidation, enableIntrospection, queryExecution)
      }
    case GET(
          p"/api/graphql" ? q_o"query=$query" & q_o"variables=$variables" & q_o"operationName=$operation" & q_o"extensions=$extensions"
        ) if allowGETRequests =>
      makeGetAction(interpreter, skipValidation, enableIntrospection, queryExecution)(
        query,
        variables,
        operation,
        extensions
      )
    case GET(p"/ws/graphql") if subscriptions =>
      makeWebSocket(interpreter, skipValidation, enableIntrospection, keepAliveTime, queryExecution)
    case GET(p"/graphiql") if playground      =>
      actionBuilder(
        Results.Ok
          .sendResource("graphiql.html")(controllerComponents.executionContext, controllerComponents.fileMimeTypes)
      )
  }

}
