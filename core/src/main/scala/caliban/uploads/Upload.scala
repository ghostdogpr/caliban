package caliban.uploads

import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.{ GraphQLRequest, InputValue, PathValue }
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZSink
import zio.{ Chunk, RIO, Trace, UIO, URIO }

import scala.annotation.tailrec

final case class Upload(name: String)(implicit val trace: Trace) {

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

/**
 * Wraps a request which included an upload request,
 * this stores the additional information necessary to be able to resolve
 * files at query time
 */
case class GraphQLUploadRequest(
  request: GraphQLRequest,
  fileMap: List[(String, List[PathValue])], // This needs to be used to remap the input values
  fileHandle: UIO[Uploads]
) {

  /**
   * Override the variables values with their respective targets
   * @return
   */
  def remap: GraphQLRequest =
    request.copy(
      variables = request.variables.map { vars =>
        fileMap.foldLeft(vars) { case (acc, (name, rest)) =>
          val value = rest match {
            case PathValue.Key("variables") :: PathValue.Key(key) :: path =>
              acc.get(key).map(loop(_, path, name)).map(key -> _)
            case _                                                        => None
          }
          value.fold(acc)(v => acc + v)
        }
      }
    )

  /**
   * We need to continue stepping through the path until we find the point where we are supposed
   * to inject the string.
   */
  private def loop(value: InputValue, path: List[PathValue], name: String): InputValue =
    path.headOption match {
      case Some(StringValue(key))        =>
        value match {
          case InputValue.ObjectValue(fields) =>
            val v = fields.get(key).fold[InputValue](NullValue)(loop(_, path.drop(1), name))
            InputValue.ObjectValue(fields + (key -> v))
          case _                              => NullValue
        }
      case Some(IntValue.IntNumber(idx)) =>
        value match {
          case InputValue.ListValue(values) =>
            InputValue.ListValue(replaceAt(values, idx)(loop(_, path.drop(1), name)))
          case _                            => NullValue
        }
      case None                          =>
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
