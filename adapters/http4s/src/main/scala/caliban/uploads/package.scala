package caliban

import java.nio.file.Files

import zio.blocking.Blocking
import zio.stream.{ Stream, ZStream }
import zio.{ Has, Layer, UIO, ZIO, ZLayer }

package object uploads {
  type Uploads = Has[Multipart]

  object Uploads {
    val empty: Layer[Nothing, Uploads] =
      ZLayer.succeed(new Multipart {
        override def stream(name: String): ZStream[Blocking, Throwable, Byte] = Stream.empty

        override def file(name: String): ZIO[Any, Nothing, Option[FileMeta]] = ZIO.none
      })

    def stream(name: String): ZStream[Uploads with Blocking, Throwable, Byte] =
      ZStream.accessStream(_.get.stream(name))

    def fileMeta(name: String): ZIO[Uploads, Nothing, Option[FileMeta]] =
      ZIO.accessM(_.get.file(name))

    def handler(fileHandle: String => UIO[Option[FileMeta]]): ZIO[Any, Nothing, Uploads] =
      ZIO
        .succeed(new Multipart {
          override def stream(name: String): ZStream[Blocking, Throwable, Byte] =
            for {
              ref   <- ZStream.fromEffectOption(fileHandle(name).some)
              bytes <- ZStream
                         .fromInputStream(Files.newInputStream(ref.path))
            } yield bytes

          override def file(name: String): ZIO[Any, Nothing, Option[FileMeta]] =
            fileHandle(name)
        })
        .asService

  }
}
