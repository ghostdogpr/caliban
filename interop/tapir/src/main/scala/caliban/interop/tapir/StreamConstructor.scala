package caliban.interop.tapir

import zio.stream.ZStream

trait StreamConstructor[BS] {
  def apply(stream: ZStream[Any, Throwable, Byte]): BS
}

object StreamConstructor {
  implicit val zioStreams: StreamConstructor[ZStream[Any, Throwable, Byte]] =
    (stream: ZStream[Any, Throwable, Byte]) => stream

  implicit val noStreams: StreamConstructor[Nothing] =
    (_: ZStream[Any, Throwable, Byte]) => throw new UnsupportedOperationException("Streams are not supported")
}
