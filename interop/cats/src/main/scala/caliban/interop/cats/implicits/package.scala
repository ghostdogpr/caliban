package caliban.interop.cats

import caliban.execution.QueryExecution
import caliban.schema.Schema
import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.ApplicativeThrow

package object implicits {

  implicit class CatsEffectGraphQLInterpreter[R, E](private val underlying: GraphQLInterpreter[R, E]) extends AnyVal {
    def executeAsync[F[_]](
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, InputValue] = Map.empty,
      extensions: Map[String, InputValue] = Map.empty
    )(implicit interop: ToEffect[F, R]): F[GraphQLResponse[E]] =
      CatsInterop.executeAsync(underlying)(
        query,
        operationName,
        variables,
        extensions
      )

    def checkAsync[F[_]](query: String)(implicit interop: ToEffect[F, Any]): F[Unit] =
      CatsInterop.checkAsync(underlying)(query)
  }

  implicit class CatsEffectGraphQL[R](private val underlying: GraphQL[R]) extends AnyVal {
    @deprecated("Use interpreterF instead")
    def interpreterAsync[F[_]](implicit interop: ToEffect[F, Any]): F[GraphQLInterpreter[R, CalibanError]] =
      CatsInterop.interpreterAsync[F, R](underlying)

    def interpreterF[F[_]: ApplicativeThrow]: F[GraphQLInterpreter[R, CalibanError]] =
      CatsInterop.interpreterF[F, R](underlying)
  }

  implicit def catsEffectSchema[F[_], R, A](implicit interop: FromEffect[F, R], ev: Schema[R, A]): Schema[R, F[A]] =
    CatsInterop.schema
}
