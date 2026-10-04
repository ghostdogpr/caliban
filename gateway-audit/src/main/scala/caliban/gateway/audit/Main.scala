package caliban.gateway.audit

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, Supergraph }
import zio._

import java.nio.file.Paths

object Main extends ZIOAppDefault {

  private val gateway = Gateway.fromSupergraph(Supergraph.file(Paths.get("supergraph.graphql")))

  override def run =
    for {
      interpreter <- gateway.interpreter
      server      <- QuickAdapter(interpreter).runServer(4000, "/graphql")
    } yield server
}
