package caliban.gateway.subgraphs

import caliban.CalibanError.ExecutionError
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.Value.StringValue
import caliban.execution.Field
import caliban.gateway.SubGraph
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.introspection.adt.{ __Schema, __Type, Extend, TypeVisitor }
import caliban.parsing.Parser
import caliban.parsing.adt.OperationType
import caliban.tools.SttpClient
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import sttp.client3.jsoniter._
import sttp.client3.{ basicRequest, DeserializationException, HttpError, Identity, RequestT, UriContext }
import zio.{ Chunk, RIO, ZIO }

case class FederatedSubGraph(name: String, url: String, headers: Map[String, String], exposeAtRoot: Boolean)
    extends SubGraph[SttpClient] { self =>
  def build: RIO[SttpClient, SubGraphExecutor[SttpClient]] =
    for {
      res         <- ZIO
                       .serviceWithZIO[SttpClient](_.send(makeRequest(GraphQLRequest(Some("{ _service { sdl } }")))))
                       .map(_.body)
                       .absolve
      sdl          = res.asObjectValue.get("_service").asObjectValue.get("sdl") match {
                       case StringValue(value) => value
                       case _                  => ""
                     }
      doc         <- Parser.parseQuery(sdl)
      remoteSchema = RemoteSchema.parseRemoteSchema(doc)
    } yield new SubGraphExecutor[SttpClient] {
      val name: String          = self.name
      val exposeAtRoot: Boolean = self.exposeAtRoot
      val schema: __Schema      = remoteSchema

      val entities: Map[String, (String, __Type)] =
        remoteSchema.types.flatMap { t =>
          t.directives.flatMap(_.find(_.name == "key")).flatMap(_.arguments.get("fields")) match {
            case Some(StringValue(key)) => t.name.map(_ -> ((key, t)))
            case _                      => None
          }
        }.toMap

      val visitor: TypeVisitor = TypeVisitor.modify(t =>
        t.name.flatMap(entities.get) match {
          case Some((key, entityType)) =>
            t.copy(
              directives = t.directives.map(_.filterNot(_.name == "key")),
              fields = args => {
                val extraFields =
                  entityType
                    .fields(args)
                    .map(
                      _.collect {
                        case field if field.name != key =>
                          field.copy(extend =
                            Some(
                              Extend(
                                name,
                                sourceFieldName = "_entities",
                                argumentMappings = Map(
                                  key -> (v =>
                                    "representations" -> ListValue(
                                      List(
                                        ObjectValue(Map("__typename" -> StringValue(t.name.getOrElse("")), key -> v))
                                      )
                                    )
                                  )
                                ),
                                filterBatchResults = Some(_.get(key) == _.get(key)),
                                additionalFields = List(key),
                                target = t.name
                              )
                            )
                          )
                      }
                    )
                    .getOrElse(Nil)

                Some(t.fields(args).fold(extraFields)(_ ++ extraFields))
              }
            )
          case None                    => t
        }
      )

      override val visitors: Chunk[TypeVisitor] = Chunk(visitor)

      def run(field: Field, operationType: OperationType): ZIO[SttpClient, ExecutionError, ResponseValue] =
        (for {
          res <- ZIO.serviceWithZIO[SttpClient](_.send(makeRequest(field.withTypeName.toGraphQLRequest(operationType))))
          body <- ZIO.fromEither(res.body) // TODO: handle errors
        } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
    }

  private def makeRequest(
    graphQLRequest: GraphQLRequest
  ): RequestT[Identity, Either[ExecutionError, ResponseValue], Any] =
    basicRequest
      .post(uri"$url")
      .body(graphQLRequest)
      .headers(headers)
      .response(asJson[GraphQLResponse[CalibanError]])
      .mapResponse(_.map(_.data).left.map {
        case DeserializationException(body, error) =>
          ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error))
        case HttpError(_, statusCode)              => ExecutionError(s"HTTP Error: $statusCode")
      })
}
