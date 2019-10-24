package caliban.interop.cats

import caliban.parsing.adt.Value
import caliban.{ GraphQL, ResponseValue }
import cats.effect.Async
import cats.instances.either._
import cats.syntax.functor._
import zio.Runtime

object CatsInterop {

  def executeAsync[F[_]: Async, R, Q, M, S](graphQL: GraphQL[R, Q, M, S])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, Value] = Map(),
    skipValidation: Boolean = false
  )(implicit runtime: Runtime[R]): F[ResponseValue] =
    Async[F].async { cb =>
      val execution =
        graphQL.execute(query, operationName, variables, skipValidation)

      runtime.unsafeRunAsync(execution)(exit => cb(exit.toEither))
    }

  def checkAsync[F[_]: Async, R, Q, M, S](
    graphQL: GraphQL[R, Q, M, S]
  )(query: String)(implicit runtime: Runtime[R]): F[Unit] =
    Async[F].async { cb =>
      runtime.unsafeRunAsync(graphQL.execute(query))(exit => cb(exit.toEither.void))
    }
}
