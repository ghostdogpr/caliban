package caliban.uploads

import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream
import zio.{ Chunk, Trace, UIO, ULayer, URIO, ZIO, ZLayer }

trait Uploads {
  def stream(name: String): ZStream[Any, Throwable, Byte]
  def file(name: String): ZIO[Any, Nothing, Option[FileMeta]]
}

object Uploads {
  val empty: ULayer[Uploads] = {
    implicit val trace: Trace = Trace.empty
    ZLayer.succeed(new Uploads {
      def stream(name: String): ZStream[Any, Throwable, Byte] = ZStream.empty
      def file(name: String): UIO[Option[FileMeta]]           = ZIO.none
    })
  }

  def stream(name: String)(implicit trace: Trace): ZStream[Uploads, Throwable, Byte] =
    ZStream.serviceWithStream(_.stream(name))

  def fileMeta(name: String)(implicit trace: Trace): URIO[Uploads, Option[FileMeta]] =
    ZIO.serviceWithZIO(_.file(name))

  def handler(fileHandle: String => UIO[Option[FileMeta]])(implicit trace: Trace): UIO[Uploads] =
    ZIO
      .succeed(new Uploads {
        def stream(name: String): ZStream[Any, Throwable, Byte] =
          for {
            ref   <- ZStream.fromZIOOption(fileHandle(name).some)
            bytes <- ZStream.fromChunk(Chunk.fromArray(ref.bytes))
          } yield bytes

        def file(name: String): UIO[Option[FileMeta]] =
          fileHandle(name)
      })
}
