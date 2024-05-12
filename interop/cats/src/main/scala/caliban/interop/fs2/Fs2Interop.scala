package caliban.interop.fs2

import caliban.interop.cats.FromEffect
import caliban.introspection.adt.__Type
import caliban.schema.{ Schema, Step }
import fs2.Stream
import zio.stream.ZStream
import zio.stream.interop.fs2z._
import zio.{ RIO, Task }

object Fs2Interop {
  def schemaStreamRIO[R, A](implicit ev: Schema[R, ZStream[R, Throwable, A]]): Schema[R, Stream[RIO[R, *], A]] =
    new Schema[R, Stream[RIO[R, *], A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def nullable: Boolean = ev.nullable
      override def canFail: Boolean  = true

      override def resolve(value: Stream[RIO[R, *], A]): Step[R] =
        ev.resolve(value.toZStream())
    }

  def schemaStreamF[F[_], R, A](implicit
    interop: FromEffect[F, R],
    ev: Schema[R, Stream[RIO[R, *], A]]
  ): Schema[R, Stream[F, A]] =
    new Schema[R, Stream[F, A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def nullable: Boolean = ev.nullable
      override def canFail: Boolean  = true

      override def resolve(value: Stream[F, A]): Step[R] =
        ev.resolve(value.translate(interop.fromEffectK))
    }
}
