package caliban.interop.fs2

import caliban.introspection.adt.__Type
import caliban.schema.Schema
import caliban.schema.Step
import fs2.Stream
import zio.RIO
import zio.stream.ZStream
import zio.stream.interop.fs2z._

object Fs2Interop {
  def schemaStreamRIO[R, A](implicit ev: Schema[R, ZStream[R, Throwable, A]]): Schema[R, Stream[RIO[R, *], A]] =
    new Schema[R, Stream[RIO[R, *], A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def optional: Boolean = ev.optional

      override def resolve(value: Stream[RIO[R, *], A]): Step[R] =
        ev.resolve(value.toZStream())
    }
}
