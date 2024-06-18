package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.capabilities.Streams
import sttp.model._
import sttp.shared.Identity
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import zio._

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

sealed trait HttpUploadInterpreter[-R, E] { self =>
  protected def endpoint[S](
    streams: Streams[S]
  ): PublicEndpoint[UploadRequest, TapirResponse, CalibanResponse[streams.BinaryStream], S]

  protected def executeRequest[BS](
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]]

  private def parsePath(path: String): List[PathValue] = path.split('.').map(PathValue.parse).toList

  def serverEndpoint[R1 <: R, S](streams: Streams[S])(implicit
    streamConstructor: StreamConstructor[streams.BinaryStream],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): CalibanUploadsEndpoint[R1, streams.BinaryStream, S] = {
    def logic(request: UploadRequest): RIO[R1, Either[TapirResponse, CalibanResponse[streams.BinaryStream]]] = {
      val (parts, serverRequest) = request
      val partsMap               = parts.map(part => part.name -> part).toMap

      val io =
        for {
          rawOperations <- ZIO.fromOption(partsMap.get("operations")) orElseFail TapirResponse(StatusCode.BadRequest)
          request       <- requestCodec.rawDecode(new String(rawOperations.body, StandardCharsets.UTF_8)) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => ZIO.succeed(v)
                           }
          rawMap        <- ZIO.fromOption(partsMap.get("map")) orElseFail TapirResponse(StatusCode.BadRequest)
          map           <- mapCodec.rawDecode(new String(rawMap.body, StandardCharsets.UTF_8)) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => ZIO.succeed(v)
                           }
          filePaths      = map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList
                             .flatMap(kv => kv._2.map(kv._1 -> _))
          handler        = Uploads.handler(handle =>
                             ZIO
                               .succeed(partsMap.get(handle))
                               .some
                               .flatMap(fp =>
                                 Random.nextUUID.asSomeError
                                   .map(uuid =>
                                     FileMeta(
                                       uuid.toString,
                                       fp.body,
                                       fp.contentType,
                                       fp.fileName.getOrElse(""),
                                       fp.body.length
                                     )
                                   )
                               )
                               .unsome
                           )
          uploadQuery    = GraphQLUploadRequest(request, filePaths, handler)
          query          = serverRequest.headers
                             .find(isFtv1Header)
                             .fold(uploadQuery.remap)(_ => uploadQuery.remap.withFederatedTracing)
          response      <- executeRequest(query, serverRequest)
                             .provideSomeLayer[R](ZLayer(uploadQuery.fileHandle))
        } yield response

      io.either
    }

    endpoint(streams).serverLogic(logic(_))
  }

  def serverEndpointFuture[S](streams: Streams[S])(runtime: Runtime[R])(implicit
    streamConstructor: StreamConstructor[streams.BinaryStream],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): ServerEndpoint[S, Future] = {
    implicit val r: Runtime[R] = runtime
    convertHttpEndpointToFuture[R, streams.BinaryStream, S, UploadRequest](serverEndpoint(streams))
  }

  def serverEndpointIdentity[S](streams: Streams[S])(runtime: Runtime[R])(implicit
    streamConstructor: StreamConstructor[streams.BinaryStream],
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): ServerEndpoint[S, Identity] = {
    implicit val r: Runtime[R] = runtime
    convertHttpEndpointToIdentity[R, streams.BinaryStream, S, UploadRequest](serverEndpoint(streams))
  }

  def intercept[R1](interceptor: Interceptor[R1, R]): HttpUploadInterpreter[R1, E] =
    HttpUploadInterpreter.Intercepted(self, interceptor)

  def prependPath(path: List[String]): HttpUploadInterpreter[R, E] =
    HttpUploadInterpreter.Prepended(self, path)

  def configure[R1](configurator: Configurator[R1]): HttpUploadInterpreter[R & R1, E] =
    intercept[R & R1](ZLayer.scopedEnvironment[R & R1 & ServerRequest](configurator *> ZIO.environment[R]))
}

object HttpUploadInterpreter {
  private case class Base[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    responseValueCodec: JsonCodec[ResponseValue]
  ) extends HttpUploadInterpreter[R, E] {
    def endpoint[S](
      streams: Streams[S]
    ): PublicEndpoint[UploadRequest, TapirResponse, CalibanResponse[streams.BinaryStream], S] =
      makeHttpUploadEndpoint(streams)

    def executeRequest[BS](
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]] =
      interpreter.executeRequest(graphQLRequest).map(buildHttpResponse[E, BS](serverRequest))
  }

  private case class Intercepted[R1, R, E](
    interpreter: HttpUploadInterpreter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends HttpUploadInterpreter[R1, E] {
    override def intercept[R2](interceptor: Interceptor[R2, R1]): HttpUploadInterpreter[R2, E] =
      Intercepted[R2, R, E](interpreter, ZLayer.makeSome[R2 & ServerRequest, R](interceptor, layer))

    def endpoint[S](
      streams: Streams[S]
    ): PublicEndpoint[UploadRequest, TapirResponse, CalibanResponse[streams.BinaryStream], S] =
      interpreter.endpoint(streams)

    def executeRequest[BS](
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R1, TapirResponse, CalibanResponse[BS]] =
      interpreter.executeRequest(graphQLRequest, serverRequest).provideSome[R1](ZLayer.succeed(serverRequest), layer)
  }

  private case class Prepended[R, E](
    interpreter: HttpUploadInterpreter[R, E],
    path: List[String]
  ) extends HttpUploadInterpreter[R, E] {
    override def endpoint[S](
      streams: Streams[S]
    ): PublicEndpoint[UploadRequest, TapirResponse, CalibanResponse[streams.BinaryStream], S] = {
      val endpoints = interpreter.endpoint(streams)
      if (path.nonEmpty) {
        val p: List[EndpointInput[Unit]]   = path.map(stringToPath)
        val fixedPath: EndpointInput[Unit] = p.tail.foldLeft(p.head)(_ / _)

        endpoints.prependIn(fixedPath)
      } else {
        endpoints
      }
    }

    def executeRequest[BS](
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]] =
      interpreter.executeRequest(graphQLRequest, serverRequest)
  }

  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit responseValueCodec: JsonCodec[ResponseValue]): HttpUploadInterpreter[R, E] =
    Base(interpreter)

  def makeHttpUploadEndpoint[S](streams: Streams[S])(implicit
    responseValueCodec: JsonCodec[ResponseValue]
  ): PublicEndpoint[UploadRequest, TapirResponse, CalibanResponse[streams.BinaryStream], S] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(header[MediaType](HeaderNames.ContentType))
      .out(statusCode)
      .out(header[Option[String]](HeaderNames.CacheControl))
      .out(outputBody(streams))
      .errorOut(errorBody)
}
