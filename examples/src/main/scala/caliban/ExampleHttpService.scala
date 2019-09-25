package caliban

import caliban.GraphQL._
import caliban.ExampleData._
import caliban.schema.ResponseValue
import caliban.schema.ResponseValue.{ ObjectValue, StreamValue }
import fs2.{ Pipe, Stream }
import io.circe.{ Decoder, Json }
import io.circe.magnolia.derivation.decoder.semiauto._
import io.circe.parser.{ decode, parse }
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{ CORS, Logger }
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import zio.console.putStrLn
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{ IO, Task, UIO, ZIO }

object ExampleHttpService extends CatsApp {

  object dsl extends Http4sDsl[Task]
  import dsl._

  case class GraphQLRequest(query: String, operationName: Option[String])

  implicit val queryDecoder: Decoder[GraphQLRequest] = deriveMagnoliaDecoder[GraphQLRequest]

  val interpreter
    : GraphQL[Queries, Mutations, Subscriptions] = graphQL(unsafeRun(ExampleService.resolver)) // TODO replace

  def execute(query: GraphQLRequest): IO[CalibanError, ResponseValue] =
    interpreter.execute(query.query, query.operationName)

  val restService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ POST -> Root / "graphql" =>
      for {
        query    <- req.attemptAs[GraphQLRequest].value.absolve
        result   <- execute(query).fold(err => s"""{"errors":["${err.toString}"]}""", result => s"""{"data":$result}""")
        json     <- Task.fromEither(parse(result))
        response <- Ok(json)
      } yield response
  }

  def processMessage(sendQueue: fs2.concurrent.Queue[Task, WebSocketFrame]): Pipe[Task, WebSocketFrame, Unit] =
    _.collect { case Text(text, _) => text }.flatMap { text =>
      Stream.eval {
        for {
          msg     <- Task.fromEither(decode[Json](text))
          payload = msg.hcursor.downField("payload")
          id      = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
          _ <- Task.whenCase(payload.downField("query").success.flatMap(_.value.asString)) {
                case Some(query) =>
                  for {
                    result <- execute(
                               GraphQLRequest(
                                 query,
                                 payload.downField("operationName").success.flatMap(_.value.asString)
                               )
                             )
                    _ <- Task.whenCase(result) {
                          case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                            stream.foreach { item =>
                              val res = ObjectValue(List(fieldName -> item)).toString
                              sendQueue.enqueue1(
                                WebSocketFrame.Text(s"""{"id":"$id","type":"data","payload":{"data":$res}}""")
                              )
                            }.fork
                        }
                  } yield ()
              }
        } yield ()
      }
    }

  val wsService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "graphql" =>
      for {
        sendQueue <- fs2.concurrent.Queue.unbounded[Task, WebSocketFrame]
        builder <- WebSocketBuilder[Task].build(
                    sendQueue.dequeue,
                    processMessage(sendQueue),
                    headers = Headers.of(Header("Sec-WebSocket-Protocol", "graphql-ws"))
                  )
      } yield builder
  }

  val httpApp: HttpApp[Task] = Router(
    "/api" -> Logger.httpRoutes(logHeaders = true, logBody = false)(CORS(restService)),
    "/ws"  -> Logger.httpRoutes(logHeaders = true, logBody = false)(CORS(wsService))
  ).orNotFound
  val serverBuilder: BlazeServerBuilder[Task] =
    BlazeServerBuilder[Task].bindHttp(8088, "localhost").withHttpApp(httpApp)

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    serverBuilder.resource.toManaged[Any].useForever.foldM(err => putStrLn(err.toString).as(1), _ => UIO(0))
}
