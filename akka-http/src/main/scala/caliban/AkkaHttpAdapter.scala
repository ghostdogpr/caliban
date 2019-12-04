package caliban

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, Sink, Source}
import akka.stream.{ActorMaterializer, FlowShape, OverflowStrategy}
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import zio.{Runtime, URIO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object AkkaHttpAdapter extends FailFastCirceSupport {


  def execute[R, Q, M, S, E](
                                      interpreter: GraphQL[R, Q, M, S, E],
                                      query: GraphQLRequest
                                    ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  def makeRestService[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E])(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._
    post {
        entity(as[GraphQLRequest]) { request =>
          complete({
            runtime.unsafeRunToFuture(
              execute(interpreter, request)
                .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
              .map(gqlResult => HttpResponse(200, entity = HttpEntity(`application/json`, gqlResult.toString())) ))
              .future
          })
        }
      }
    }
}
