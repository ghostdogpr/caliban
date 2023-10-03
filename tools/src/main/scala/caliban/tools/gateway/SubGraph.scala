package caliban.tools.gateway

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.introspection.Introspector
import caliban.introspection.adt.__Schema
import caliban.parsing.adt.OperationType
import caliban.tools.{ Options, RemoteSchema, SchemaLoader, SttpClient }
import caliban.validation.Validator
import caliban.{ CalibanError, GraphQL, GraphQLResponse, ResponseValue }
import sttp.client3.jsoniter._
import sttp.client3.{ basicRequest, DeserializationException, HttpError, Identity, RequestT, UriContext }
import zio.{ RIO, ZIO }

abstract class SubGraph[-R](val name: String) {
  def build: RIO[R, SubGraphData[R]]
}

object SubGraph {
  def graphQL(name: String, url: String, headers: Map[String, String] = Map.empty): SubGraph[SttpClient] =
    new SubGraph[SttpClient](name) { self =>
      def makeRequest(field: Field): RequestT[Identity, Either[ExecutionError, ResponseValue], Any] =
        basicRequest
          .post(uri"$url")
          .body(field.withTypeName.toGraphQLRequest(OperationType.Query))
          .headers(headers)
          .response(asJson[GraphQLResponse[CalibanError]])
          .mapResponse(_.map(_.data).left.map {
            case DeserializationException(body, error) =>
              ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error))
            case HttpError(_, statusCode)              => ExecutionError(s"HTTP Error: $statusCode")
          })

      def build: RIO[SttpClient, SubGraphData[SttpClient]] =
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
        } yield new SubGraphData[SttpClient](self.name, remoteSchema) {
          def run(field: Field): ZIO[SttpClient, ExecutionError, ResponseValue] =
            (for {
              res  <- ZIO.serviceWithZIO[SttpClient](_.send(makeRequest(field)))
              body <- ZIO.fromEither(res.body)
            } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
        }
    }

  def caliban[R](name: String, api: GraphQL[R]): SubGraph[R] =
    new SubGraph[R](name) { self =>
      def build: RIO[R, SubGraphData[R]] =
        for {
          interpreter  <- api.interpreter
          schemaBuilder = api.getSchemaBuilder
          rootSchema   <- Validator.validateSchema(schemaBuilder)
        } yield new SubGraphData[R](
          self.name,
          __Schema(
            schemaBuilder.schemaDescription,
            rootSchema.query.opType,
            rootSchema.mutation.map(_.opType),
            rootSchema.subscription.map(_.opType),
            schemaBuilder.types,
            Introspector.directives ++ api.getAdditionalDirectives
          )
        ) {
          def run(field: Field): ZIO[R, ExecutionError, ResponseValue] =
            interpreter
              .executeRequest(field.toGraphQLRequest(OperationType.Query))
              .map(_.toResponseValue)
        }
    }
}

private[caliban] abstract class SubGraphData[-R](val name: String, val schema: __Schema) {
  def run(field: Field): ZIO[R, ExecutionError, ResponseValue]
}
