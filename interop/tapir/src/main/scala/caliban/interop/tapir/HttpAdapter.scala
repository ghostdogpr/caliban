package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import sttp.model.{ headers => _, _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{ headers, _ }
import zio._

trait HttpAdapter[-R, E] { self =>

  protected val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]]

  def serverEndpoints[R1 <: R]: List[ServerEndpoint[Any, RIO[R1, *]]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
      val (graphQLRequest, serverRequest) = request
      executeRequest(graphQLRequest, serverRequest).either
    }
    endpoints.map(_.serverLogic(logic))
  }

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, GraphQLResponse[E]]

  def configure[R1](configurator: ZLayer[R1 & ServerRequest, TapirResponse, R]): HttpAdapter[R1, E] =
    new HttpAdapter[R1, E] {
      val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
        self.endpoints

      def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R1, TapirResponse, GraphQLResponse[E]] =
        self
          .executeRequest(graphQLRequest, serverRequest)
          .provideSome[R1](ZLayer.succeed(serverRequest), configurator)
    }
}

object HttpAdapter {
  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit requestCodec: JsonCodec[GraphQLRequest], responseCodec: JsonCodec[GraphQLResponse[E]]): HttpAdapter[R, E] =
    new HttpAdapter[R, E] {
      val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
        makeHttpEndpoints

      def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R, TapirResponse, GraphQLResponse[E]] =
        interpreter.executeRequest(graphQLRequest)
    }

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
