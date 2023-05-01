package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import sttp.capabilities.zio.ZioStreams
import sttp.model.{ headers => _, _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.model.ServerRequest
import sttp.tapir._
import zio._

sealed trait HttpInterpreter[-R, E] { self =>
  protected val endpoints: List[
    PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams]
  ]

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, CalibanResponse]

  def serverEndpoints[R1 <: R]: List[CalibanEndpoint[R1]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R1, Either[TapirResponse, CalibanResponse]] = {
      val (graphQLRequest, serverRequest) = request
      executeRequest(graphQLRequest, serverRequest).either
    }
    endpoints.map(_.serverLogic(logic))
  }

  def intercept[R1](interceptor: Interceptor[R1, R]): HttpInterpreter[R1, E] =
    HttpInterpreter.Configured(self, interceptor)

  def configure[R1](configurator: Configurator[R1]): HttpInterpreter[R & R1, E] =
    intercept[R & R1](ZLayer.scopedEnvironment[R & R1](configurator *> ZIO.environment[R]))
}

object HttpInterpreter {
  private case class Base[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ) extends HttpInterpreter[R, E] {
    val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams]] =
      makeHttpEndpoints

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R, TapirResponse, CalibanResponse] =
      interpreter.executeRequest(graphQLRequest).map(buildHttpResponse)
  }

  private case class Configured[R1, R, E](
    interpreter: HttpInterpreter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends HttpInterpreter[R1, E] {
    override def intercept[R2](interceptor: Interceptor[R2, R1]): HttpInterpreter[R2, E] =
      Configured[R2, R, E](interpreter, ZLayer.makeSome[R2 & ServerRequest, R](interceptor, layer))

    val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams]] =
      interpreter.endpoints

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R1, TapirResponse, CalibanResponse] =
      interpreter.executeRequest(graphQLRequest, serverRequest).provideSome[R1](ZLayer.succeed(serverRequest), layer)
  }

  def apply[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ): HttpInterpreter[R, E] =
    Base(interpreter)

  def makeHttpEndpoints(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams]] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams] =
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
        .out(header[MediaType](HeaderNames.ContentType))
        .out(outputBody)
        .errorOut(errorBody)

    val getEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams] =
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
        .out(header[MediaType](HeaderNames.ContentType))
        .out(outputBody)
        .errorOut(errorBody)

    postEndpoint :: getEndpoint :: Nil
  }
}
