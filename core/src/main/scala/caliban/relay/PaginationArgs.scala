package caliban.relay

import caliban.CalibanError
import zio.Exit
import zio.prelude.Validation

object Pagination {
  import PaginationCount._
  import PaginationCursor._

  def apply[C: Cursor](args: PaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    apply(args.first, args.last, args.before, args.after)

  def apply[C: Cursor](args: ForwardPaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    (args.first match {
      case None    => Validation.fail("first cannot be empty")
      case Some(a) => validatePositive("first", a).map(First.apply)
    })
      .zipWith(args.after match {
        case None    => Validation.succeed(NoCursor)
        case Some(x) => Validation.fromEither(Cursor[C].decode(x)).map(After.apply)
      })(new Pagination[C](_, _))
      .toExit

  def apply[C: Cursor](args: BackwardPaginationArgs[C]): Exit[CalibanError, Pagination[C]] =
    (args.last match {
      case None    => Validation.fail("last cannot be empty")
      case Some(a) => validatePositive("last", a).map(Last.apply)
    })
      .zipWith(args.before match {
        case None    => Validation.succeed(NoCursor)
        case Some(x) => Validation.fromEither(Cursor[C].decode(x)).map(Before.apply)
      })(new Pagination[C](_, _))
      .toExit

  def apply[C: Cursor](
    first: Option[Int],
    last: Option[Int],
    before: Option[String],
    after: Option[String]
  ): Exit[CalibanError, Pagination[C]] =
    validateFirstLast(first, last)
      .zipWith(validateCursors(before, after))(new Pagination[C](_, _))
      .toExit

  private def validateCursors[C: Cursor](
    before: Option[String],
    after: Option[String]
  ): Validation[String, PaginationCursor[C]] =
    (before, after) match {
      case (Some(_), Some(_)) =>
        Validation.fail("before and after cannot both be set")
      case (Some(x), _)       =>
        Validation.fromEither(Cursor[C].decode(x)).map(Before.apply)
      case (_, Some(x))       =>
        Validation.fromEither(Cursor[C].decode(x)).map(After.apply)
      case (None, None)       => Validation.succeed(NoCursor)
    }

  private def validateFirstLast(first: Option[Int], last: Option[Int]) =
    (first, last) match {
      case (None, None)       =>
        Validation.fail("first and last cannot both be empty")
      case (Some(_), Some(_)) =>
        Validation.fail("first and last cannot both be set")
      case (Some(a), _)       =>
        validatePositive("first", a).map(First.apply)
      case (_, Some(b))       =>
        validatePositive("last", b).map(Last.apply)
    }

  private def validatePositive(which: String, i: Int) =
    if (i > -1) Validation.succeed(i) else Validation.fail(s"$which cannot be negative")

  private implicit class ValidationOps[E, A](private val v: Validation[String, A]) extends AnyVal {
    def toExit: Exit[CalibanError, A] = {
      val either = v.toEitherWith(errors => CalibanError.ExecutionError(msg = errors.mkString(", ")))
      Exit.fromEither(either)
    }
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
