package caliban.stitching

import caliban.ResponseValue.ObjectValue
import caliban.execution.Field
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, RemoteMutation, RemoteQuery, ResponseValue }
import sttp.client4.ResponseException.{ DeserializationException, UnexpectedStatusCode }
import sttp.client4._
import sttp.client4.jsoniter._
import zio._

case class RemoteResolver[-R, +E, -A, +B](
  run: A => ZIO[R, E, B]
) { self =>
  def mapM[R1 <: R, E1 >: E, A1 <: A, C1](bc: B => ZIO[R1, E1, C1]): RemoteResolver[R1, E1, A, C1] =
    RemoteResolver((x: A) => self.run(x).flatMap(bc))

  def map[C](bc: B => C): RemoteResolver[R, E, A, C] = RemoteResolver((x: A) => self.run(x).map(bc))

  def >>>[R1 <: R, E1 >: E, C](
    other: RemoteResolver[R1, E1, B, C]
  ): RemoteResolver[R1, E1, A, C] =
    RemoteResolver((a: A) => self.run(a).flatMap(other.run(_)))
}

object RemoteResolver {
  def fromFunction[A, B](f: A => B): RemoteResolver[Any, Nothing, A, B] = RemoteResolver((a: A) => ZIO.succeed(f(a)))

  def fromFunctionM[A, R, E, B](f: A => ZIO[R, E, B]): RemoteResolver[R, E, A, B] = RemoteResolver((a: A) => f(a))

  def fromEffect[A, R, E, B](effect: ZIO[R, E, B]): RemoteResolver[R, E, A, B] = RemoteResolver((_: Any) => effect)

  def fromUrl(apiUrl: String): RemoteResolver[Backend[Task], CalibanError.ExecutionError, Field, ResponseValue] =
    toQuery >>> request(apiUrl) >>> execute >>> unwrap

  def request(apiUrl: String): RemoteResolver[Any, CalibanError.ExecutionError, GraphQLRequest, HttpRequest] =
    RemoteResolver.fromFunctionM((q: GraphQLRequest) =>
      ZIO.succeed(
        basicRequest
          .post(uri"$apiUrl")
          .body(asJson(q))
          .response(asJson[GraphQLResponse[CalibanError]])
          .mapResponse(resp =>
            resp.fold(
              {
                case DeserializationException(body, error, _) =>
                  Left(CalibanError.ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error)))
                case UnexpectedStatusCode(_, statusCode)      =>
                  Left(CalibanError.ExecutionError(s"HTTP Error: $statusCode"))
              },
              resp => Right(resp.data)
            )
          )
      )
    )

  def execute: RemoteResolver[Backend[Task], CalibanError.ExecutionError, HttpRequest, ResponseValue] =
    RemoteResolver.fromFunctionM((r: HttpRequest) =>
      (for {
        res  <- ZIO.serviceWithZIO[Backend[Task]](_.send(r))
        body <- ZIO.fromEither(res.body)
      } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
    )

  val toQuery: RemoteResolver[Any, Nothing, Field, GraphQLRequest] =
    RemoteResolver.fromFunction((f: Field) => RemoteQuery(f).toGraphQLRequest)

  val toMutation: RemoteResolver[Any, Nothing, Field, GraphQLRequest] =
    RemoteResolver.fromFunction((f: Field) => RemoteMutation(f).toGraphQLRequest)

  val unwrap: RemoteResolver[Any, Nothing, ResponseValue, ResponseValue] =
    RemoteResolver.fromFunction {
      case v @ ObjectValue(fields) => fields.headOption.map(_._2).getOrElse(v)
      case x                       => x
    }
}
