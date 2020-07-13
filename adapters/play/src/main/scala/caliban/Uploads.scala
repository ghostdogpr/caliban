package caliban

import java.nio.file.{ Files, Path }

import caliban.InputValue.ListValue
import caliban.Uploads.{ FileMeta, Uploads }
import caliban.Value.{ NullValue, StringValue }
import caliban.schema.Annotations.GQLName
import zio.blocking.Blocking
import zio.stream.{ Stream, ZSink, ZStream }
import zio.{ Chunk, Has, Layer, RIO, UIO, URIO, ZIO, ZLayer }

@GQLName("Upload")
sealed case class Upload(name: String) {

  val allBytes: RIO[Uploads with Blocking, Chunk[Byte]] =
    Uploads.stream(name).run(ZSink.foldLeftChunks(Chunk[Byte]())(_ ++ (_: Chunk[Byte])))

  val meta: URIO[Uploads, Option[FileMeta]] =
    Uploads.fileMeta(name)

}

object Uploads {
  type Uploads = Has[Service.Service]

  def stream(name: String): ZStream[Uploads with Blocking, Throwable, Byte] =
    ZStream.accessStream(_.get.stream(name))

  def fileMeta(name: String): ZIO[Uploads, Nothing, Option[FileMeta]] =
    ZIO.accessM(_.get.file(name))

  case class FileMeta(
    id: String,
    path: Path,
    dispositionType: String,
    contentType: Option[String],
    fileName: String,
    fileSize: Long
  )

  object Service {
    val empty: Layer[Nothing, Uploads] =
      ZLayer.succeed(new Service {
        override def stream(name: String): ZStream[Blocking, Throwable, Byte] = Stream.empty

        override def file(name: String): ZIO[Any, Nothing, Option[FileMeta]] = ZIO.none
      })

    def handler(fileHandle: String => UIO[Option[FileMeta]]): ZIO[Any, Nothing, Uploads] =
      ZIO
        .succeed(new Service {
          override def stream(name: String): ZStream[Blocking, Throwable, Byte] =
            for {
              ref <- ZStream.fromEffectOption(fileHandle(name).some)
              bytes <- ZStream
                        .fromInputStream(Files.newInputStream(ref.path))
            } yield bytes

          override def file(name: String): ZIO[Any, Nothing, Option[FileMeta]] =
            fileHandle(name)
        })
        .asService

    trait Service {
      def stream(name: String): ZStream[Blocking, Throwable, Byte]
      def file(name: String): ZIO[Any, Nothing, Option[FileMeta]]
    }
  }
}

/**
 * Wraps a request which included an upload request,
 * this stores the additional information necessary to be able to resolve
 * files at query time
 */
case class GraphQLUploadRequest(
  request: GraphQLRequest,
  fileMap: List[(String, List[Either[String, Int]])], // This needs to be used to remap the input values
  fileHandle: UIO[Uploads]
) {

  /**
   * Override the variables values with their respective targets
   * @return
   */
  def remap: GraphQLRequest =
    request.copy(
      variables = request.variables.map { vars =>
        val files = fileMap.flatMap {
          case (name, Left("variables") :: Left(key) :: path) => vars.get(key).map(loop(_, path, name)).map(key -> _)
          case _                                              => None
        }

        vars ++ files.groupBy(_._1).map {
          case (key, values) if values.lengthCompare(1) > 0 => (key, ListValue(values.map(_._2)))
          case (key, List(value))                           => (key, value._2)
        }

//        files.foldLeft(vars) {
//          case (acc, (key, StringValue(value))) => acc.get(key) match {
//            case Some(ListValue(mv)) => acc.updated(key, mv.patch(value, List(mv), 1))
//          }
//          case (acc, _) => acc
//
//        }
//        vars ++ files
      }
    )

  /**
   * We need to continue stepping through the path until we find the point where we are supposed
   * to inject the string.
   */
  private def loop(value: InputValue, path: List[Either[String, Int]], name: String): InputValue =
    path.headOption match {
      case Some(Left(key)) =>
        value match {
          case InputValue.ObjectValue(fields) =>
            fields.get(key).fold[InputValue](NullValue)(loop(_, path.drop(1), name))
          case _ => NullValue
        }
      case Some(Right(idx)) =>
        value match {
          case InputValue.ListValue(values) =>
            values.lift(idx).fold[InputValue](NullValue)(loop(_, path.drop(1), name))
          case _ => NullValue
        }
      case None =>
        // If we are out of values then we are at the end of the path, so we need to replace this current node
        // with a string node containing the file name
        StringValue(name)
    }

}

object GraphQLUploadRequest {}
