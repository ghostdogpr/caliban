package caliban.relay

import caliban.CalibanError
import zio._

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
    validateFirstLast(first, last)
      .validate(
        validateCursors(before, after)
      )
      .map { case (count, cursor) => new Pagination[C](count, cursor) }
      .parallelErrors
      .mapError((errors: ::[String]) => CalibanError.ExecutionError(msg = errors.mkString(", ")))

  private def validateCursors[C: Cursor](
    before: Option[String],
    after: Option[String]
  ): ZIO[Any, String, PaginationCursor[C]] =
    (before, after) match {
      case (Some(_), Some(_)) =>
        ZIO.fail("before and after cannot both be set")
      case (Some(x), _)       =>
        ZIO.fromEither(Cursor[C].decode(x)).map(Before(_))
      case (_, Some(x))       =>
        ZIO.fromEither(Cursor[C].decode(x)).map(After(_))
      case (None, None)       => ZIO.succeed(NoCursor)
    }

  private def validateFirstLast(first: Option[Int], last: Option[Int]) =
    (first, last) match {
      case (None, None)       =>
        ZIO.fail("first and last cannot both be empty")
      case (Some(_), Some(_)) =>
        ZIO.fail("first and last cannot both be set")
      case (Some(a), _)       =>
        validatePositive("first", a).map(First(_))
      case (_, Some(b))       =>
        validatePositive("last", b).map(Last(_))
    }

  private def validatePositive(which: String, i: Int) =
    ZIO.cond(i > -1, i, s"$which cannot be negative")
}

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
