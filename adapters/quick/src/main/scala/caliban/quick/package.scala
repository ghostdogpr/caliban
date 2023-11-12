package caliban

import zio.RIO

package object quick {

  implicit class GraphqlServerOps[R](val gql: GraphQL[R]) extends AnyVal {
    def runServer(port: Int, api: String, graphiql: Option[String] = None): RIO[R, Nothing] =
      gql.interpreter.flatMap(QuickAdapter(_).runServer(port, api, graphiql))
  }

}
