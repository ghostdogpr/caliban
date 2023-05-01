package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.model.{ headers => _, _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import zio._

import java.nio.charset.StandardCharsets
import scala.util.Try

sealed trait HttpUploadInterpreter[-R, E] { self =>
  protected val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any]

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, GraphQLResponse[E]]

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  def serverEndpoint[R1 <: R](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]]
  ): ServerEndpoint[Any, RIO[R1, *]] = {
    def logic(request: UploadRequest): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
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

    endpoint.serverLogic(logic)
  }

  def configure[R1](configurator: ZLayer[R1 & ServerRequest, TapirResponse, R]): HttpUploadInterpreter[R1, E] =
    HttpUploadInterpreter.Configured(self, configurator)

  def configure(configurator: URIO[Scope, Unit]): HttpUploadInterpreter[R, E] =
    configure[R](ZLayer.scopedEnvironment[R](configurator *> ZIO.environment[R]))
}

object HttpUploadInterpreter {
  private case class Base[R, E](interpreter: GraphQLInterpreter[R, E])(implicit
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ) extends HttpUploadInterpreter[R, E] {
    val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
      makeHttpUploadEndpoint

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R, TapirResponse, GraphQLResponse[E]] =
      interpreter.executeRequest(graphQLRequest)
  }

  private case class Configured[R1, R, E](
    interpreter: HttpUploadInterpreter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends HttpUploadInterpreter[R1, E] {
    override def configure[R2](
      configurator: ZLayer[R2 & ServerRequest, TapirResponse, R1]
    ): HttpUploadInterpreter[R2, E] =
      Configured[R2, R, E](interpreter, ZLayer.makeSome[R2 & ServerRequest, R](configurator, layer))

    val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
      interpreter.endpoint

    def executeRequest(
      graphQLRequest: GraphQLRequest,
      serverRequest: ServerRequest
    ): ZIO[R1, TapirResponse, GraphQLResponse[E]] =
      interpreter.executeRequest(graphQLRequest, serverRequest).provideSome[R1](ZLayer.succeed(serverRequest), layer)
  }

  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit responseCodec: JsonCodec[GraphQLResponse[E]]): HttpUploadInterpreter[R, E] =
    Base(interpreter)

  def makeHttpUploadEndpoint[E](implicit
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(customCodecJsonBody[GraphQLResponse[E]])
      .errorOut(errorBody)
}
