package caliban.uploads

import caliban.InputValue.ListValue
import caliban.Value.{ NullValue, StringValue }
import caliban.{ GraphQLRequest, InputValue }
import scala.annotation.tailrec
import zio.stream.{ ZSink, ZStream }
import zio.{ Chunk, RIO, UIO, URIO, ZIO }

final case class Upload(name: String) {
  val allBytes: RIO[Uploads, Chunk[Byte]] =
    Uploads.stream(name).run(ZSink.foldLeftChunks(Chunk[Byte]())(_ ++ (_: Chunk[Byte])))

  val meta: URIO[Uploads, Option[FileMeta]] =
    Uploads.fileMeta(name)
}

case class FileMeta(
  id: String,
  bytes: Array[Byte],
  contentType: Option[String],
  fileName: String,
  fileSize: Long
)

trait Multipart {
  def stream(name: String): ZStream[Any, Throwable, Byte]
  def file(name: String): ZIO[Any, Nothing, Option[FileMeta]]
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
          case (key, value :: Nil) => (key, value._2)
          case (key, values)       => (key, ListValue(values.map(_._2)))
        }
      }
    )

  /**
   * We need to continue stepping through the path until we find the point where we are supposed
   * to inject the string.
   */
  private def loop(value: InputValue, path: List[Either[String, Int]], name: String): InputValue =
    path.headOption match {
      case Some(Left(key))  =>
        value match {
          case InputValue.ObjectValue(fields) =>
            InputValue.ObjectValue(
              fields + (key ->
                fields.get(key).fold[InputValue](NullValue)(loop(_, path.drop(1), name)))
            )
          case _                              => NullValue
        }
      case Some(Right(idx)) =>
        value match {
          case InputValue.ListValue(values) =>
            InputValue.ListValue(replaceAt(values, idx)(loop(_, path.drop(1), name)))
          case _                            => NullValue
        }
      case None             =>
        // If we are out of values then we are at the end of the path, so we need to replace this current node
        // with a string node containing the file name
        StringValue(name)
    }

  private def replaceAt[A](xs: List[A], idx: Int)(f: A => A): List[A] = {
    @tailrec
    def loop[A](xs: List[A], idx: Int, acc: List[A], f: A => A): List[A] =
      (xs, idx) match {
        case (x :: xs, 0)   => (f(x) :: acc).reverse ++ xs
        case (Nil, _)       => acc.reverse
        case (x :: xs, idx) => loop(xs, idx - 1, x :: acc, f)
      }

    loop(xs, idx, List(), f)
  }

}
