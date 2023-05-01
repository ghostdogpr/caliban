package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import sttp.model.{ headers => _, _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{ headers, _ }
import zio._

sealed trait HttpAdapter[-R, E] { self =>
  protected val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]]

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, GraphQLResponse[E]]

  def serverEndpoints[R1 <: R]: List[ServerEndpoint[Any, RIO[R1, *]]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
      val (graphQLRequest, serverRequest) = request
      executeRequest(graphQLRequest, serverRequest).either
    }
    endpoints.map(_.serverLogic(logic))
  }

  def configure[R1](configurator: ZLayer[R1 & ServerRequest, TapirResponse, R]): HttpAdapter[R1, E] =
    HttpAdapter.Configured(self, configurator)

  def configure[R1](configurator: URIO[R1 & Scope, Unit]): HttpAdapter[R & R1, E] =
    configure[R & R1](ZLayer.scopedEnvironment[R & R1](configurator *> ZIO.environment[R]))
}

object HttpAdapter {
  private case class Base[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ) extends HttpAdapter[R, E] {
    val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
      makeHttpEndpoints

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R, TapirResponse, GraphQLResponse[E]] =
      interpreter.executeRequest(graphQLRequest)
  }

  private case class Configured[R1, R, E](
    adapter: HttpAdapter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends HttpAdapter[R1, E] {
    override def configure[R2](configurator: ZLayer[R2 & ServerRequest, TapirResponse, R1]): HttpAdapter[R2, E] =
      Configured[R2, R, E](adapter, ZLayer.makeSome[R2 & ServerRequest, R](configurator, layer))

    val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
      adapter.endpoints

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R1, TapirResponse, GraphQLResponse[E]] =
      adapter.executeRequest(graphQLRequest, serverRequest).provideSome[R1](ZLayer.succeed(serverRequest), layer)
  }

  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit requestCodec: JsonCodec[GraphQLRequest], responseCodec: JsonCodec[GraphQLResponse[E]]): HttpAdapter[R, E] =
    Base(interpreter)

  def makeHttpEndpoints[E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any] =
      endpoint.post
        .in(
          (headers and stringBody and queryParams).mapDecode { case (headers, body, params) =>
            val getRequest =
              if (params.get("query").isDefined)
                queryFromQueryParams(params)
              else if (
                headers.exists(header =>
                  header.name.equalsIgnoreCase(HeaderNames.ContentType) &&
                    MediaType
                      .parse(header.value)
                      .exists(mediaType => mediaType.mainType == "application" && mediaType.subType == "graphql")
                )
              )
                DecodeResult.Value(GraphQLRequest(query = Some(body)))
              else requestCodec.decode(body)

            getRequest.map(request => headers.find(isFtv1Header).fold(request)(_ => request.withFederatedTracing))
          }(request => (Nil, requestCodec.encode(request), QueryParams()))
        )
        .in(extractFromRequest(identity))
        .out(customCodecJsonBody[GraphQLResponse[E]])
        .errorOut(errorBody)

    val getEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any] =
      endpoint.get
        .in(
          queryParams.mapDecode(queryFromQueryParams)(request =>
            QueryParams.fromMap(
              Map(
                "query"         -> request.query.getOrElse(""),
                "operationName" -> request.operationName.getOrElse(""),
                "variables"     -> request.variables
                  .map(_.map { case (k, v) => s""""$k":${v.toInputString}""" }.mkString("{", ",", "}"))
                  .getOrElse(""),
                "extensions"    -> request.extensions
                  .map(_.map { case (k, v) => s""""$k":${v.toInputString}""" }.mkString("{", ",", "}"))
                  .getOrElse("")
              ).filter { case (_, v) => v.nonEmpty }
            )
          )
        )
        .in(extractFromRequest(identity))
        .out(customCodecJsonBody[GraphQLResponse[E]])
        .errorOut(errorBody)

    postEndpoint :: getEndpoint :: Nil
  }
}
