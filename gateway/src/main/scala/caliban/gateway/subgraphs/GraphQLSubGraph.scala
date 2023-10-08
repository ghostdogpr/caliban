package caliban.gateway.subgraphs

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.gateway.SubGraph
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.introspection.adt.__Schema
import caliban.parsing.adt.OperationType
import caliban.tools.{ Options, SchemaLoader, SttpClient }
import caliban.{ CalibanError, GraphQLResponse, ResponseValue }
import sttp.client3.jsoniter._
import sttp.client3.{ basicRequest, DeserializationException, HttpError, Identity, RequestT, UriContext }
import zio.{ RIO, ZIO }

case class GraphQLSubGraph(name: String, url: String, headers: Map[String, String], exposeAtRoot: Boolean)
    extends SubGraph[SttpClient] { self =>
  def build: RIO[SttpClient, SubGraphExecutor[SttpClient]] =
    for {
      doc          <- SchemaLoader
                        .fromIntrospection(
                          url,
                          Some(headers.map { case (k, v) => Options.Header(k, v) }.toList),
                          supportIsRepeatable = false
                        )
                        .load
      remoteSchema <- ZIO
                        .succeed(RemoteSchema.parseRemoteSchema(doc))
                        .someOrFail(new Throwable("Failed to parse remote schema"))
    } yield new SubGraphExecutor[SttpClient] {
      val name: String          = self.name
      val exposeAtRoot: Boolean = self.exposeAtRoot
      val schema: __Schema      = remoteSchema

      def run(field: Field): ZIO[SttpClient, ExecutionError, ResponseValue] =
        (for {
          res <- ZIO.serviceWithZIO[SttpClient](_.send(makeRequest(field)))
          body <- ZIO.fromEither(res.body) // TODO: handle errors
        } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
    }

  private def makeRequest(field: Field): RequestT[Identity, Either[ExecutionError, ResponseValue], Any] =
    basicRequest
      .post(uri"$url")
      .body(field.withTypeName.toGraphQLRequest(OperationType.Query)) // TODO: other operation types
      .headers(headers)
      .response(asJson[GraphQLResponse[CalibanError]])
      .mapResponse(_.map(_.data).left.map {
        case DeserializationException(body, error) =>
          ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error))
        case HttpError(_, statusCode)              => ExecutionError(s"HTTP Error: $statusCode")
      })

}
