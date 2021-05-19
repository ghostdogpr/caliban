package caliban.tools.stitching

import zio._

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import caliban.execution.Field
import caliban.ResponseValue.ObjectValue

import sttp.client3._
import sttp.client3.circe._
import sttp.client3.asynchttpclient.zio._

class DefaultResolvers(apiURL: String) {
  val toQuery: RemoteResolver[Any, Nothing, Field, GraphQLRequest] =
    RemoteResolver.fromFunction((f: Field) => RemoteQuery(f).toGraphQLRequest)

  val unwrap: RemoteResolver[Any, Nothing, ResponseValue, ResponseValue] =
    RemoteResolver.fromFunction((v: ResponseValue) =>
      v match {
        case ObjectValue(fields) => fields.headOption.map(_._2).getOrElse(v)
        case x                   => x
      }
    )

  val request: RemoteResolver[Any, CalibanError.ExecutionError, GraphQLRequest, HttpRequest] =
    RemoteResolver((q: GraphQLRequest) =>
      ZIO.succeed(
        basicRequest
          .post(uri"$apiURL")
          .body(q)
          .response(asJson[GraphQLResponse[CalibanError]])
          .mapResponse(resp =>
            resp.fold(
              err =>
                err match {
                  case DeserializationException(body, error) =>
                    Left(CalibanError.ExecutionError(s"${error.getMessage()}: ${body}"))
                  case HttpError(body, statusCode)           => Left(CalibanError.ExecutionError(s"HTTP Error: $statusCode"))
                },
              resp => Right(resp.data)
            )
          )
      )
    )

  def execute: RemoteResolver[SttpClient, CalibanError.ExecutionError, HttpRequest, ResponseValue] =
    RemoteResolver.fromEffect((r: HttpRequest) =>
      (for {
        res  <- send(r)
        body <- ZIO.fromEither(res.body)
      } yield body).mapError(e => CalibanError.ExecutionError(e.toString))
    )

  val default: RemoteResolver[SttpClient, CalibanError.ExecutionError, Field, ResponseValue] =
    toQuery >>> request >>> execute >>> unwrap
}
