package caliban.interop.cats

import caliban.parsing.adt.Value
import caliban.{GraphQL, ResponseValue}
import cats.effect.Async
import zio.Runtime

package object implicits {

  implicit class CatsEffectGraphQL[R, Q, M, S](
    underlying: GraphQL[R, Q, M, S]
  ) {

    def executeAsync[F[_]: Async](
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, Value] = Map(),
      skipValidation: Boolean = false
    )(implicit runtime: Runtime[R]): F[ResponseValue] =
      CatsInterop.executeAsync(underlying)(
        query,
        operationName,
        variables,
        skipValidation
      )
  }
}
