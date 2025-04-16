package caliban.validation

import caliban.CalibanError.ValidationError

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[caliban] object ValidationOps {
  import caliban.syntax._

  val unit: Either[Nothing, Unit] = Right(())

  def when(cond: Boolean)(f: => Either[ValidationError, Unit]): Either[ValidationError, Unit] =
    if (cond) f else unit

  // NOTE: We overload instead of using `Iterable` to avoid interface method calls
  def validateAllDiscard[A](
    in: List[A]
  )(f: A => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    val nil = Nil
    var rem = in
    while (rem ne nil) {
      val res = f(rem.head)
      if (res.isLeft) return res
      else rem = rem.tail
    }
    unit
  }

  def validateAllDiscard[K, V](
    in: Map[K, V]
  )(f: (K, V) => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    val it = in.iterator
    while (it.hasNext) {
      val kv  = it.next()
      val res = f(kv._1, kv._2)
      if (res.isLeft) return res
    }
    unit
  }

  def validateAllDiscard[A](
    in: mutable.Set[A]
  )(f: A => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    val it = in.iterator
    while (it.hasNext) {
      val res = f(it.next())
      if (res.isLeft) return res
    }
    unit
  }

  def validateAllNonEmpty[A](
    in: List[A]
  )(f: A => Either[ValidationError, Unit]): Option[Either[ValidationError, Unit]] =
    if (in eq Nil) None
    else Some(validateAllDiscard(in)(f))

  def validateAll[A, B](
    in: List[A]
  )(f: A => Either[ValidationError, B]): Either[ValidationError, List[B]] = {
    val nil     = Nil
    var rem     = in
    val builder = ListBuffer.empty[B]

    while (rem ne nil) {
      f(rem.head) match {
        case Right(v) => builder.addOne(v)
        case left     => return left.asInstanceOf[Either[ValidationError, List[B]]]
      }
      rem = rem.tail
    }
    Right(builder.result())
  }

  def failWhen(
    condition: Boolean
  )(msg: => String, explanatoryText: => String): Either[ValidationError, Unit] =
    if (condition) Validator.failValidation(msg, explanatoryText) else unit

  final implicit class EitherOps[E, A](private val self: Either[E, A]) extends AnyVal {
    def *>[B](other: Either[E, B]): Either[E, B] =
      (self: @unchecked) match {
        case l: Left[E @unchecked, B @unchecked] => l
        case _                                   => other
      }

    def as[B](b: B): Either[E, B] =
      (self: @unchecked) match {
        case l: Left[E @unchecked, B @unchecked] => l
        case _                                   => Right(b)
      }

    def unit: Either[E, Unit] =
      (self: @unchecked) match {
        case l: Left[E @unchecked, Unit @unchecked] => l
        case _                                      => ValidationOps.unit
      }
  }
}
