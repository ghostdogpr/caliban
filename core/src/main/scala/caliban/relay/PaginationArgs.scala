package caliban.relay

import caliban.CalibanError
import zio.Exit

object Pagination {
  import PaginationCount._
  import PaginationCursor._

  def apply[C: Cursor](args: PaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    apply(args.first, args.last, args.before, args.after)

  def apply[C: Cursor](args: ForwardPaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    toPaginationExit(
      args.first match {
        case None    => Left("first cannot be empty")
        case Some(a) => validatePositive("first", a).map(First.apply)
      },
      args.after match {
        case None    => Right(NoCursor)
        case Some(x) => Cursor[C].decode(x).map(After.apply)
      }
    )

  def apply[C: Cursor](args: BackwardPaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    toPaginationExit(
      args.last match {
        case None    => Left("last cannot be empty")
        case Some(a) => validatePositive("last", a).map(Last.apply)
      },
      args.before match {
        case None    => Right(NoCursor)
        case Some(x) => Cursor[C].decode(x).map(Before.apply)
      }
    )

  def apply[C: Cursor](
    first: Option[Int],
    last: Option[Int],
    before: Option[String],
    after: Option[String]
  ): Exit[CalibanError, Pagination[C]] =
    toPaginationExit(
      validateFirstLast(first, last),
      validateCursors(before, after)
    )

  private def validateCursors[C: Cursor](
    before: Option[String],
    after: Option[String]
  ): Either[String, PaginationCursor[C]] =
    (before, after) match {
      case (Some(_), Some(_)) =>
        Left("before and after cannot both be set")
      case (Some(x), _)       =>
        Cursor[C].decode(x).map(Before.apply)
      case (_, Some(x))       =>
        Cursor[C].decode(x).map(After.apply)
      case (None, None)       => Right(NoCursor)
    }

  private def validateFirstLast(first: Option[Int], last: Option[Int]) =
    (first, last) match {
      case (None, None)       =>
        Left("first and last cannot both be empty")
      case (Some(_), Some(_)) =>
        Left("first and last cannot both be set")
      case (Some(a), _)       =>
        validatePositive("first", a).map(First.apply)
      case (_, Some(b))       =>
        validatePositive("last", b).map(Last.apply)
    }

  private def validatePositive(which: String, i: Int) =
    if (i > -1) Right(i) else Left(s"$which cannot be negative")

  private def toPaginationExit[C](
    count: Either[String, PaginationCount],
    cursor: Either[String, PaginationCursor[C]]
  ): Exit[CalibanError, Pagination[C]] = {
    val v = (count, cursor) match {
      case (Right(c), Right(r)) => Right(new Pagination(c, r))
      case (Left(c), Left(r))   => Left(List(c, r))
      case (Left(c), _)         => Left(List(c))
      case (_, Left(r))         => Left(List(r))
    }
    Exit.fromEither(v.left.map(errors => CalibanError.ExecutionError(msg = errors.mkString(", "))))
  }

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

  def toPagination: Exit[CalibanError, Pagination[C]] = Pagination(self)
}

abstract class ForwardPaginationArgs[C: Cursor] { self =>
  val first: Option[Int]
  val after: Option[String]

  def toPagination: Exit[CalibanError, Pagination[C]] = Pagination(self)
}

abstract class BackwardPaginationArgs[C: Cursor] { self =>
  val last: Option[Int]
  val before: Option[String]

  def toPagination: Exit[CalibanError, Pagination[C]] = Pagination(self)
}

case class PaginationError(reason: String)
