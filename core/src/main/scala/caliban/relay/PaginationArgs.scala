package caliban.relay

import caliban.CalibanError
import zio._
import zio.stacktracer.TracingImplicits.disableAutoTrace

object Pagination {
  import PaginationCount._
  import PaginationCursor._

  def apply[C: Cursor](
    args: PaginationArgs[C]
  )(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] =
    apply(args.first, args.last, args.before, args.after)

  def apply[C: Cursor](
    args: ForwardPaginationArgs[C]
  )(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] =
    (args.first match {
      case None    => ZIO.fail(s"first cannot be empty")
      case Some(a) => validatePositive("first", a).map(First(_))
    })
      .validate(args.after match {
        case None    => ZIO.succeed(NoCursor)
        case Some(x) => ZIO.fromEither(Cursor[C].decode(x)).map(After(_))
      })
      .map { case (count, cursor) => new Pagination[C](count, cursor) }
      .parallelErrors
      .mapError((errors: ::[String]) => CalibanError.ExecutionError(msg = errors.mkString(", ")))

  def apply[C: Cursor](
    args: BackwardPaginationArgs[C]
  )(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] =
    (args.last match {
      case None    => ZIO.fail(s"last cannot be empty")
      case Some(a) => validatePositive("last", a).map(Last(_))
    })
      .validate(args.before match {
        case None    => ZIO.succeed(NoCursor)
        case Some(x) => ZIO.fromEither(Cursor[C].decode(x)).map(Before(_))
      })
      .map { case (count, cursor) => new Pagination[C](count, cursor) }
      .parallelErrors
      .mapError((errors: ::[String]) => CalibanError.ExecutionError(msg = errors.mkString(", ")))

  def apply[C: Cursor](
    first: Option[Int],
    last: Option[Int],
    before: Option[String],
    after: Option[String]
  )(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] =
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
  )(implicit trace: Trace): ZIO[Any, String, PaginationCursor[C]] =
    (before, after) match {
      case (Some(_), Some(_)) =>
        ZIO.fail("before and after cannot both be set")
      case (Some(x), _)       =>
        ZIO.fromEither(Cursor[C].decode(x)).map(Before(_))
      case (_, Some(x))       =>
        ZIO.fromEither(Cursor[C].decode(x)).map(After(_))
      case (None, None)       => ZIO.succeed(NoCursor)
    }

  private def validateFirstLast(first: Option[Int], last: Option[Int])(implicit trace: Trace) =
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

  private def validatePositive(which: String, i: Int)(implicit trace: Trace) =
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

  def toPagination(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] = Pagination(
    self
  )
}

abstract class ForwardPaginationArgs[C: Cursor] { self =>
  val first: Option[Int]
  val after: Option[String]

  def toPagination(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] = Pagination(
    self
  )
}

abstract class BackwardPaginationArgs[C: Cursor] { self =>
  val last: Option[Int]
  val before: Option[String]

  def toPagination(implicit trace: Trace): ZIO[Any, CalibanError, Pagination[C]] = Pagination(
    self
  )
}

case class PaginationError(reason: String)
