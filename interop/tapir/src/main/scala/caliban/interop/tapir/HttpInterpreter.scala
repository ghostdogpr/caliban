package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import sttp.capabilities.Streams
import sttp.model.{ headers => _, _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.model.ServerRequest
import sttp.tapir._
import zio._

sealed trait HttpInterpreter[-R, E] { self =>

  protected def endpoints[S](streams: Streams[S]): List[
    PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S]
  ]

  protected def executeRequest[BS](
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]]

  /**
   * Intercepts the request via a [[zio.ZLayer]], eliminating part of the environment and potentially failing the request with a [[TapirResponse]].
   *
   * Note that for cases where the eliminating part of the environment is not required, consider using [[interceptWithZIO]] instead.
   */
  def intercept[R1](interceptor: Interceptor[R1, R]): HttpInterpreter[R1, E]

  def interceptWithZIO[R1](
    interceptor: ServerRequest => ZIO[R1, TapirResponse, Unit]
  )(implicit tag: Tag[R1]): HttpInterpreter[R & R1, E]

  def configure[R1](configurator: Configurator[R1])(implicit tag: Tag[R1]): HttpInterpreter[R & R1, E]

  def serverEndpoints[R1 <: R, S](stream: Streams[S])(implicit
    streamConstructor: StreamConstructor[stream.BinaryStream]
  ): List[CalibanEndpoint[R1, stream.BinaryStream, S]] = {
    def logic(
      request: (GraphQLRequest, ServerRequest)
    ): RIO[R1, Either[TapirResponse, CalibanResponse[stream.BinaryStream]]] = {
      val (graphQLRequest, serverRequest) = request
      executeRequest(graphQLRequest, serverRequest).either
    }
    endpoints[S](stream).map(_.serverLogic(logic(_)))
  }

}

object HttpInterpreter {
  private case class Base[R, E](
    interpreter: GraphQLInterpreter[R, E],
    config: Option[Configurator[R]]
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ) extends HttpInterpreter[R, E] {
    def endpoints[S](
      streams: Streams[S]
    ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S]] =
      makeHttpEndpoints(streams)

    def executeRequest[BS](
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]] = {
      def exec = interpreter.executeRequest(graphQLRequest).map(buildHttpResponse[E, BS])
      config match {
        case Some(cfg) => ZIO.scoped[R](cfg *> exec)
        case None      => exec
      }
    }

    def intercept[R1](interceptor: Interceptor[R1, R]): HttpInterpreter[R1, E] =
      HttpInterpreter.Intercepted(this, interceptor)

    def interceptWithZIO[R1](
      interceptor: ServerRequest => ZIO[R1, TapirResponse, Unit]
    )(implicit tag: Tag[R1]): HttpInterpreter[R & R1, E] =
      HttpInterpreter.Intercepted[R & R1, R, E](
        this,
        ZLayer.environment[R] ++ ZLayer(ZIO.serviceWithZIO[ServerRequest](interceptor))
      )

    def configure[R1](configurator: Configurator[R1])(implicit tag: Tag[R1]): HttpInterpreter[R & R1, E] =
      copy[R & R1, E](config = config match {
        case Some(cfg) => Some(cfg *> configurator)
        case None      => Some(configurator)
      })
  }

  private case class Intercepted[R1, R, E](
    interpreter: HttpInterpreter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends HttpInterpreter[R1, E] {

    def intercept[R2](interceptor: Interceptor[R2, R1]): HttpInterpreter[R2, E] =
      copy[R2, R, E](layer = ZLayer.makeSome[R2 & ServerRequest, R](interceptor, layer))

    def interceptWithZIO[R2](
      interceptor: ServerRequest => ZIO[R2, TapirResponse, Unit]
    )(implicit tag: Tag[R2]): HttpInterpreter[R1 & R2, E] =
      copy[R1 & R2, R & R2, E](layer =
        layer ++ ZLayer.environment[R2] ++ ZLayer(ZIO.serviceWithZIO[ServerRequest](interceptor))
      )

    def configure[R2](configurator: Configurator[R2])(implicit tag: Tag[R2]): HttpInterpreter[R1 & R2, E] =
      Intercepted[R1 & R2, R & R2, E](
        interpreter.configure[R2](configurator),
        layer ++ ZLayer.environment[R2]
      )

    def endpoints[S](
      streams: Streams[S]
    ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S]] =
      interpreter.endpoints(streams)

    def executeRequest[BS](
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R1, TapirResponse, CalibanResponse[BS]] =
      interpreter.executeRequest(graphQLRequest, serverRequest).provideSome[R1](ZLayer.succeed(serverRequest), layer)
  }

  def apply[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ): HttpInterpreter[R, E] =
    Base(interpreter, None)

  def makeHttpEndpoints[S](streams: Streams[S])(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseValueCodec: JsonCodec[ResponseValue]
  ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S]] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint
      : PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S] =
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
        .out(outputBody(streams))
        .errorOut(errorBody)

    val getEndpoint
      : PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[streams.BinaryStream], S] =
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
        .out(outputBody(streams))
        .errorOut(errorBody)

    postEndpoint :: getEndpoint :: Nil
  }
}
