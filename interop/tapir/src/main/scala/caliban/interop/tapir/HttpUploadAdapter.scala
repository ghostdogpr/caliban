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

trait HttpUploadAdapter[-R, E] { self =>

  protected val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any]

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

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, GraphQLResponse[E]]

  def configure[R1](configurator: ZLayer[R1 & ServerRequest, TapirResponse, R]): HttpUploadAdapter[R1, E] =
    new HttpUploadAdapter[R1, E] {
      val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
        self.endpoint

      def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R1, TapirResponse, GraphQLResponse[E]] =
        self
          .executeRequest(graphQLRequest, serverRequest)
          .provideSome[R1](ZLayer.succeed(serverRequest), configurator)
    }
}

object HttpUploadAdapter {
  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit responseCodec: JsonCodec[GraphQLResponse[E]]): HttpUploadAdapter[R, E] =
    new HttpUploadAdapter[R, E] {
      val endpoint: PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
        makeHttpUploadEndpoint

      def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R, TapirResponse, GraphQLResponse[E]] =
        interpreter.executeRequest(graphQLRequest)
    }

  def makeHttpUploadEndpoint[E](implicit
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(customCodecJsonBody[GraphQLResponse[E]])
      .errorOut(errorBody)
}
