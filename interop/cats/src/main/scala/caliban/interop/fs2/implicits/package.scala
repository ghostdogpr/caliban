package caliban.interop.fs2

import caliban.schema.Schema
import fs2.Stream
import zio.RIO
import zio.stream.ZStream

package object implicits {

  implicit def fs2SchemaStreamRIO[R, A](implicit
    ev: Schema[R, ZStream[R, Throwable, A]]
  ): Schema[R, Stream[RIO[R, *], A]] =
    Fs2Interop.schemaStreamRIO
}
