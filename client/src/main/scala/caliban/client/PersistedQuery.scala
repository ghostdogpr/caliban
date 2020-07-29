package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError, PersistedQueryNotFound, ServerError }
import caliban.client.Operations.IsOperation
import caliban.client.Value.{ valueDecoder, NumberValue, ObjectValue, StringValue }
import sttp.client._
import sttp.model.Uri
import sttp.client.circe._
import io.circe.parser
import sttp.client.monad.MonadError
import sttp.client.monad.syntax._

import scala.language.higherKinds

sealed trait PersistedQuery[A] {

  def apply[F[_], S, WS[_]](
    uri: Uri
  )(implicit backend: SttpBackend[F, S, WS]): F[Response[Either[CalibanClientError, A]]]

}

object PersistedQuery {
  import java.security.MessageDigest

  def apply[A, Origin: IsOperation](
    queryName: String,
    selectionBuilder: SelectionBuilder[A, Origin],
    useVariables: Boolean = false,
    extensions: Option[ObjectValue] = None
  ): PersistedQuery[A] = new PersistedQuery[A] {
    val md          = MessageDigest.getInstance("SHA-256")
    val query       = selectionBuilder.toGraphQL[A, Origin](useVariables, Some(queryName), extensions)
    val hashedQuery = new String(md.digest(query.query.getBytes("UTF-8")))
    val persistedExtension: (String, Value) = "persistedQuery" ->
      ObjectValue(
        List(
          "version"    -> NumberValue(1),
          "sha256Hash" -> StringValue(hashedQuery)
        )
      )
    val optimisticQuery: Value = ObjectValue(
      List(
        "operationName" -> StringValue(queryName),
        "variables"     -> ObjectValue(query.variables.toList),
        "extensions" -> ObjectValue(
          persistedExtension :: extensions.fold(List.empty[(String, Value)])(_.fields)
        )
      )
    )

    val optimisticRequest = basicRequest
      .body(optimisticQuery)
      .mapResponse { response =>
        for {
          resp          <- response.left.map(CommunicationError(_))
          parsed        <- parser.decode[GraphQLResponse](resp).left.map(ex => DecodingError("Json deserialization error"))
          (apq, normal) = parsed.errors.partition(_.message.eq("PersistedQueryNotFound"))
          _             <- Either.cond(apq.nonEmpty, (), PersistedQueryNotFound)
          data          <- Either.cond(normal.nonEmpty, parsed.data, ServerError(parsed.errors))
          objectValue <- data match {
                          case Some(o: ObjectValue) => Right(o)
                          case _                    => Left(DecodingError("Result is not an object"))
                        }
          result <- selectionBuilder.fromGraphQL(objectValue)
        } yield result
      }

    override def apply[F[_], S, WS[_]](
      uri: Uri
    )(implicit backend: SttpBackend[F, S, WS]): F[Response[Either[CalibanClientError, A]]] = {
      implicit val F: MonadError[F] = backend.responseMonad

      lazy val secondRequest = selectionBuilder.toRequestWithExtensions[A, Origin](
        uri,
        useVariables,
        Some(queryName),
        extensions = Some(
          ObjectValue(
            persistedExtension :: extensions.fold(List.empty[(String, Value)])(_.fields)
          )
        )
      )

      for {
        optimisticResp <- optimisticRequest.post(uri).send()
        resp <- optimisticResp.body match {
                 case Right(value)                 => F.unit(Right(value))
                 case Left(PersistedQueryNotFound) => secondRequest.send()
               }
      } yield resp

    }
  }
}
