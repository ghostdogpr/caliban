package caliban.interop.cats

import caliban.schema.Schema
import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.effect.{ Async, Effect }
import zio.Runtime

package object implicits {

  implicit class CatsEffectGraphQLInterpreter[R, E](underlying: GraphQLInterpreter[R, E]) {

    def executeAsync[F[_]: Async](
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, InputValue] = Map(),
      skipValidation: Boolean = false
    )(implicit runtime: Runtime[R]): F[GraphQLResponse[E]] =
      CatsInterop.executeAsync(underlying)(
        query,
        operationName,
        variables,
        skipValidation
      )

    def checkAsync[F[_]: Async](query: String)(implicit runtime: Runtime[R]): F[Unit] =
      CatsInterop.checkAsync(underlying)(query)
  }

  implicit class CatsEffectGraphQL[R, E](underlying: GraphQL[R]) {

    def interpreterAsync[F[_]: Async](implicit runtime: Runtime[R]): F[GraphQLInterpreter[R, CalibanError]] =
      CatsInterop.interpreterAsync(underlying)
  }

  implicit def effectSchema[F[_]: Effect, R, A](implicit ev: Schema[R, A]): Schema[R, F[A]] =
    CatsInterop.schema
}
