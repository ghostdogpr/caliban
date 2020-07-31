package caliban.client.apq

import caliban.client.CalibanClientError.{
  ClientError,
  CommunicationError,
  DecodingError,
  PersistedQueryNotFound,
  ServerError
}
import caliban.client.Operations.IsOperation
import caliban.client.Value.{ NullValue, NumberValue, ObjectValue, StringValue }
import caliban.client.apq.hash.CryptoHasher
import caliban.client.{ CalibanClientError, GraphQLResponse, SelectionBuilder, Value }
import io.circe.parser
import sttp.client._
import sttp.client.circe._
import sttp.client.monad.MonadError
import sttp.client.monad.syntax._
import sttp.model.Uri

import scala.language.higherKinds
import scala.util.Try

sealed trait PersistedQuery[A] {

  def apply[F[_], S, WS[_]](
    uri: Uri
  )(implicit backend: SttpBackend[F, S, WS]): F[Response[Either[CalibanClientError, A]]]

}

object PersistedQuery {
  def apply[Origin: IsOperation, A](
    selectionBuilder: SelectionBuilder[Origin, A],
    queryName: Option[String] = None,
    useVariables: Boolean = false,
    extensions: Option[ObjectValue] = None
  ): PersistedQuery[A] = new PersistedQuery[A] {
    private lazy val query = selectionBuilder.toGraphQL[A, Origin](useVariables, queryName, extensions)

    override def apply[F[_], S, WS[_]](
      uri: Uri
    )(implicit backend: SttpBackend[F, S, WS]): F[Response[Either[CalibanClientError, A]]] = {
      implicit val F: MonadError[F] = backend.responseMonad

      for {
        hashedQuery <- F.fromTry(unsafeHashQuery(query.query))
        apq         = buildExtension(hashedQuery)
        operation = ObjectValue(
          List(
            "operationName" -> queryName.fold[Value](NullValue)(StringValue),
            "variables"     -> ObjectValue(query.variables.toList),
            "extensions" -> ObjectValue(
              apq :: extensions.fold(List.empty[(String, Value)])(_.fields)
            )
          )
        )
        optimisticResp <- happyPath(operation).post(uri).send()
        resp <- optimisticResp.body match {
                 case Left(PersistedQueryNotFound) => missingHashQuery(uri, apq).send()
                 case _                            => F.unit(optimisticResp)
               }
      } yield resp

    }

    private def buildExtension(hashedQuery: String): (String, Value) =
      "persistedQuery" ->
        ObjectValue(
          List(
            "version"    -> NumberValue(1),
            "sha256Hash" -> StringValue(hashedQuery)
          )
        )

    private def happyPath(query: Value) =
      basicRequest
        .body(query)
        .mapResponse { response =>
          for {
            resp <- response.left.map(CommunicationError(_))
            parsed <- parser
                       .decode[GraphQLResponse](resp)
                       .left
                       .map(ex => DecodingError("Json deserialization error", Some(ex)))
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

    private def missingHashQuery(uri: Uri, setQueryExt: (String, Value)) = selectionBuilder.toRequest(
      uri,
      useVariables,
      queryName,
      Some(ObjectValue(setQueryExt :: extensions.fold(List.empty[(String, Value)])(_.fields)))
    )

    private def unsafeHashQuery(query: String): Try[String] =
      Try(CryptoHasher.Sha256(query.getBytes("UTF-8")).left.map(ClientError(_)).toTry).flatten
  }
}
