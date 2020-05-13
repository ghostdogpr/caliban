package caliban.play

import play.api.Mode
import play.api.mvc.{ DefaultControllerComponents, Handler, RequestHeader , Results}
import play.api.routing.sird._
import play.core.server.AkkaHttpServer
import play.core.server.ServerConfig


import scala.io.StdIn.readLine


object ExampleApp extends App {
  val server = AkkaHttpServer.fromRouterWithComponents(
    ServerConfig(
      mode = Mode.Dev,
      port = Some(8088),
      address = "127.0.0.1"
    )
  ) { components =>
    val controller = new CalibanController(
      DefaultControllerComponents(
        components.defaultActionBuilder,
        components.playBodyParsers,
        components.messagesApi,
        components.langs,
        components.fileMimeTypes,
        components.executionContext
      )
    )(components.actorSystem, components.materializer)
    val router: PartialFunction[RequestHeader, Handler] = {
      case POST(p"/api/graphql") => controller.graphqlPost
      case GET(
          p"/api/graphql" ? q"query=$query" & q_o"variables=$variables" & q_o"operation=$operation" & q_o"extensions=$extensions"
          ) =>
        controller.graphqlGet(query, variables, operation, extensions)
      case GET(p"/ws/graphql") => controller.webSocket()
      case GET(p"/graphiql") =>
        components.defaultActionBuilder(
          Results.Ok.sendResource("graphiql.html")(components.executionContext, components.fileMimeTypes)
        )
    }
    router
  }

  println("Server online at http://localhost:8088/\nPress RETURN to stop...")
  readLine()
  server.stop()

}
