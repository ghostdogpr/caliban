package caliban

import zio.{ RIO, Trace, ZIO }
import zio.http.{ RequestHandler, Response }

package object quick {

  implicit class GraphqlServerOps[R](val gql: GraphQL[R]) extends AnyVal {
    def runServer(port: Int, api: String, graphiql: Option[String] = None)(implicit trace: Trace): RIO[R, Nothing] =
      gql.interpreter.flatMap(QuickAdapter(_).runServer(port, api, graphiql))

    def handler(implicit trace: Trace): ZIO[R, CalibanError.ValidationError, RequestHandler[R, Response]] =
      gql.interpreter.map(QuickAdapter(_).handler)
  }

}
