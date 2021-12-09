package caliban.relay

import caliban.CalibanError
import zio._

sealed trait PaginationCount extends Product with Serializable {
  def count: Int
}
object PaginationCount {
  case class First(count: Int) extends PaginationCount
  case class Last(count: Int)  extends PaginationCount
}

sealed trait PaginationCursor[+C]
object PaginationCursor {
  case class After[C](cursor: C)  extends PaginationCursor[C]
  case class Before[C](cursor: C) extends PaginationCursor[C]
  case object NoCursor            extends PaginationCursor[Nothing]
}

case class Pagination[+C](
  count: PaginationCount,
  cursor: PaginationCursor[C]
)

abstract class PaginationArgs[C: Cursor] { self =>
  val first: Option[Int]
  val last: Option[Int]
  val before: Option[String]
  val after: Option[String]

  def toPagination: ZIO[Any, CalibanError, Pagination[C]] = Pagination(
    self
  )
}

case class PaginationError(reason: String)

object Pagination {
  import PaginationCount._
  import PaginationCursor._

  def apply[C: Cursor](
    args: PaginationArgs[C]
  ): ZIO[Any, CalibanError, Pagination[C]] =
    apply(args.first, args.last, args.before, args.after)

  def apply[C: Cursor](
    first: Option[Int],
    last: Option[Int],
    before: Option[String],
    after: Option[String]
  ): ZIO[Any, CalibanError, Pagination[C]] =
    Validation
      .validateWith(
        validateFirstLast(first, last),
        validateCursors(before, after)
      ) { (count, cursor) =>
        new Pagination[C](
          count,
          cursor
        )
      }
      .fold(
        errs =>
          ZIO.fail(
            CalibanError
              .ValidationError(
                msg = errs.mkString(", "),
                explanatoryText = ""
              )
          ),
        x => ZIO.succeed(x)
      )

  private def validateCursors[C: Cursor](
    before: Option[String],
    after: Option[String]
  ) =
    (before, after) match {
      case (Some(_), Some(_)) =>
        Validation.fail("both before and after may not be set")
      case (Some(x), _)       =>
        Validation.fromEither(Cursor[C].decode(x)).map(Before(_))
      case (_, Some(x))       =>
        Validation.fromEither(Cursor[C].decode(x)).map(After(_))
      case (None, None)       => Validation.succeed(NoCursor)
    }

  private def validateFirstLast(first: Option[Int], last: Option[Int]) =
    (first, last) match {
      case (None, None)       =>
        Validation.fail("first and last cannot both be empty")
      case (Some(_), Some(_)) =>
        Validation.fail("both first and last cannot be set")
      case (Some(a), _)       =>
        validatePositive("first", a).map(First(_))
      case (_, Some(b))       =>
        validatePositive("last", b).map(Last(_))
    }

  private def validatePositive(which: String, i: Int) =
    if (i < 0) Validation.fail(s"$which cannot be negative")
    else Validation.succeed(i)
}
