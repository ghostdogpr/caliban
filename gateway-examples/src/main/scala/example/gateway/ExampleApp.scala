package example.gateway

import caliban.{ GraphQLInterpreter, QuickAdapter }
import zio._
import zio.http._

abstract class ExampleApp(name: String, port: Int) extends ZIOAppDefault {
  val endpoint: URL = url"http://localhost/graphql".port(port)

  def interpreter: ZIO[Scope, Throwable, GraphQLInterpreter[Any, Any]]

  final def run: RIO[Scope, Nothing] =
    interpreter.flatMap { interpreter =>
      Console.printLine(s"$name: ${endpoint.path("/graphiql").encode}") *>
        QuickAdapter[Any](interpreter).runServer(port, endpoint.path.encode, graphiqlPath = Some("/graphiql"))
    }
}
