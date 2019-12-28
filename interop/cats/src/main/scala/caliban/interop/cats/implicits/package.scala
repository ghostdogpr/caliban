package caliban.interop.cats

import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ Schema, Step }
import caliban.{ GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.data.OptionT
import cats.effect.{ Async, Effect }
import zio.stream.ZStream
import zio.{ Runtime, Task }
import zquery.ZQuery
import cats.effect.implicits._
import zio.interop.catz._

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
  }

  implicit class CatsEffectGraphQL[R, E](underlying: GraphQL[R]) {

    def checkAsync[F[_]: Async](query: String)(implicit runtime: Runtime[R]): F[Unit] =
      CatsInterop.checkAsync(underlying)(query)
  }

  implicit def effectSchema[F[_]: Effect, R, A](implicit ev: Schema[R, A]): Schema[R, F[A]] =
    CatsInterop.schema

  implicit def optionTSchema[F[_]: Effect, R, A](implicit ev: Schema[R, Option[A]]): Schema[R, OptionT[F, A]] =
    new Schema[R, OptionT[F, A]] {
      override def toType(isInput: Boolean): __Type =
        ev.toType(isInput)

      override def optional: Boolean =
        ev.optional

      override def resolve(value: OptionT[F, A]): Step[R] =
        QueryStep(ZQuery.fromEffect(value.value.toIO.to[Task].map(ev.resolve)))
    }

  implicit def fs2StreamSchema[F[_]: Effect, R, A](
    implicit ev: Schema[R, ZStream[R, Throwable, A]]
  ): Schema[R, fs2.Stream[F, A]] =
    new Schema[R, fs2.Stream[F, A]] {
      override def optional: Boolean                        = ev.optional
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: fs2.Stream[F, A]): Step[R] =
        ev.resolve(ZStream.fromIterator(value.compile.toVector.toIO.to[Task].map(_.iterator)))
    }
}
