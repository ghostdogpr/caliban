package caliban

import zio.stream.{ Stream, ZStream }
import zio.{ Chunk, UIO, ULayer, URIO, ZIO, ZLayer }

package object uploads {
  trait Uploads {
    def stream(name: String): ZStream[Any, Throwable, Byte]
    def file(name: String): ZIO[Any, Nothing, Option[FileMeta]]
  }

  object Uploads {
    val empty: ULayer[Uploads] =
      ZLayer.succeed(new Uploads {
        def stream(name: String): ZStream[Any, Throwable, Byte] = Stream.empty
        def file(name: String): UIO[Option[FileMeta]]           = ZIO.none
      })

    def stream(name: String): ZStream[Uploads, Throwable, Byte] =
      ZStream.serviceWithStream(_.stream(name))

    def fileMeta(name: String): URIO[Uploads, Option[FileMeta]] =
      ZIO.serviceWithZIO(_.file(name))

    def handler(fileHandle: String => UIO[Option[FileMeta]]): UIO[Uploads] =
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
}
