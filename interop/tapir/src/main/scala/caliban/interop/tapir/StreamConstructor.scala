package caliban.interop.tapir

import sttp.capabilities.Streams
import zio.stream.ZStream

trait StreamConstructor[BS] {
  def apply(stream: ZStream[Any, Throwable, Byte]): BS
}

object StreamConstructor {
  implicit val zioStreams: StreamConstructor[ZStream[Any, Throwable, Byte]] =
    new StreamConstructor[ZStream[Any, Throwable, Byte]] {
      override def apply(stream: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, Byte] = stream
    }
}
