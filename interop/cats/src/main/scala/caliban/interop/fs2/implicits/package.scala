package caliban.interop.fs2

import caliban.interop.cats.FromEffect
import caliban.schema.Schema
import fs2.Stream
import zio.RIO
import zio.stream.ZStream

package object implicits {

  implicit def fs2SchemaStreamRIO[R, A](implicit
    ev: Schema[R, ZStream[R, Throwable, A]]
  ): Schema[R, Stream[RIO[R, *], A]] =
    Fs2Interop.schemaStreamRIO

  implicit def fs2SchemaStreamF[F[_], R, A](implicit
    interop: FromEffect[F, R],
    ev: Schema[R, Stream[RIO[R, *], A]]
  ): Schema[R, Stream[F, A]] =
    Fs2Interop.schemaStreamF
}
