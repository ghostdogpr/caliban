package caliban.interop.cats

import caliban.introspection.adt.__Type
import caliban.parsing.adt.Value
import caliban.schema.Step.EffectStep
import caliban.schema.{ GenericSchema, Schema, Step }
import caliban.{ GraphQL, ResponseValue }
import cats.effect.implicits._
import cats.effect.{ Async, Effect }
import cats.instances.either._
import cats.syntax.functor._
import zio.interop.catz._
import zio.{ Runtime, _ }

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

  def schema[F[_]: Effect, R, A](implicit ev: Schema[R, A]): Schema[R, F[A]] =
    new Schema[R, F[A]] {
      override def toType(isInput: Boolean): __Type =
        ev.toType(isInput)

      override def optional: Boolean =
        ev.optional

      override def resolve(value: F[A]): Step[R] =
        EffectStep(
          value.toIO
            .to[Task]
            .bimap(GenericSchema.effectfulExecutionError, ev.resolve)
        )
    }
}
