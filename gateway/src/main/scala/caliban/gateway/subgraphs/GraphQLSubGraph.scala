package caliban.gateway.subgraphs

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.gateway.SubGraph
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.introspection.adt.__Schema
import caliban.parsing.adt.OperationType
import caliban.tools.{ Header, SchemaLoader }
import caliban.{ CalibanError, GraphQLResponse, ResponseValue }
import sttp.client4.ResponseException.{ DeserializationException, UnexpectedStatusCode }
import sttp.client4._
import sttp.client4.jsoniter._
import zio.{ RIO, Task, ZIO }

case class GraphQLSubGraph(name: String, url: String, headers: Map[String, String], exposeAtRoot: Boolean)
    extends SubGraph[Backend[Task]] { self =>
  def build: RIO[Backend[Task], SubGraphExecutor[Backend[Task]]] =
    for {
      doc          <- SchemaLoader
                        .fromIntrospectionWith(url, Some(headers.map { case (k, v) => Header(k, v) }.toList))(
                          _.supportIsRepeatable(false)
                        )
                        .load
      remoteSchema <- ZIO
                        .fromOption(RemoteSchema.parseRemoteSchema(doc))
                        .orElseFail(new RuntimeException(s"No query type found in schema for subgraph $name"))
    } yield new SubGraphExecutor[Backend[Task]] {
      val name: String          = self.name
      val exposeAtRoot: Boolean = self.exposeAtRoot
      val schema: __Schema      = remoteSchema

      def run(field: Field, operationType: OperationType): ZIO[Backend[Task], ExecutionError, ResponseValue] =
        (for {
          res  <- ZIO.serviceWithZIO[Backend[Task]](_.send(makeRequest(field, operationType)))
          body <- ZIO.fromEither(res.body) // TODO: handle errors
        } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
    }

  private def makeRequest(
    field: Field,
    operationType: OperationType
  ): Request[Either[ExecutionError, ResponseValue]] =
    basicRequest
      .post(uri"$url")
      .body(asJson(field.withTypeName.toGraphQLRequest(operationType)))
      .headers(headers)
      .response(asJson[GraphQLResponse[CalibanError]])
      .mapResponse(
        _.fold(
          {
            case DeserializationException(body, error, _) =>
              Left(ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error)))
            case UnexpectedStatusCode(_, statusCode)      => Left(ExecutionError(s"HTTP Error: $statusCode"))
          },
          response =>
            if (response.errors.isEmpty) Right(response.data)
            else Left(ExecutionError(response.errors.map(_.msg).mkString("Upstream errors: ", ", ", "")))
        )
      )

}
