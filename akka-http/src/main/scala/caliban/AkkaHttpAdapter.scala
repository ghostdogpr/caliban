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

  //todo graphql websocket isn't implemented right now.
  def makeWebSocketService[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E])(implicit ec: ExecutionContext, actorSystem: ActorSystem, runtime: Runtime[R]): Route = {
  import akka.http.scaladsl.server.Directives._
    get {
      val handler = actorSystem.actorOf(GQLWebsocketHandler.props(interpreter))
      val futureFlow = (handler ? GetWebsocketFlow) (3.seconds).mapTo[Flow[Message, Message, _]]

      onComplete(futureFlow) {
        case Success(flow) => handleWebSocketMessages(flow)
        case Failure(err) => complete(err.toString)
      }
    }
  }

  case object GetWebsocketFlow
  object GQLWebsocketHandler {
    def props[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E]) = Props(new GQLWebsocketHandler(interpreter))
  }
  class GQLWebsocketHandler[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E]) extends Actor {

    implicit val actorSystem = context.system
    implicit val actorMaterializer = ActorMaterializer()

    val (down, publisher) = Source
      .actorRef[String](1000, OverflowStrategy.fail)
      .toMat(Sink.asPublisher(fanout = false))(Keep.both)
      .run()


    override def receive = {
      case GetWebsocketFlow =>

        val flow = Flow.fromGraph(GraphDSL.create() { implicit builder =>
          val textMsgFlow = builder.add(Flow[Message]
            .mapAsync(1) {
              case textMessage: TextMessage => textMessage.toStrict(3.seconds).map(_.text)
              case binaryMessage: BinaryMessage =>
                binaryMessage.dataStream.runWith(Sink.ignore)
                Future.failed(new Exception("not supported.."))
            })

          val pubSrc = builder.add(Source.fromPublisher(publisher).map(TextMessage(_)))

          textMsgFlow ~> Sink.foreach[String](self ! _)
          FlowShape(textMsgFlow.in, pubSrc.out)
        })

        sender ! flow

      case data: String =>
        down ! s"message data was $data"
    }
  }
}
