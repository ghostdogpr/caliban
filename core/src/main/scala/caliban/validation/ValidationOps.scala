package caliban.validation

import caliban.CalibanError.ValidationError

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[caliban] object ValidationOps {
  val unitR: Either[ValidationError, Unit] = Right(())

  def when(cond: Boolean)(f: => Either[ValidationError, Unit]): Either[ValidationError, Unit] =
    if (cond) f else unitR

  // NOTE: We overload instead of using `Iterable` to avoid interface method calls
  def validateAllDiscard[A](
    in: List[A]
  )(f: A => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    var rem = in
    while (rem ne Nil) {
      val res = f(rem.head)
      if (res.isLeft) return res
      else rem = rem.tail
    }
    unitR
  }

  def validateAllDiscard[K, V](
    in: Map[K, V]
  )(f: (K, V) => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    val it = in.iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      val res    = f(k, v)
      if (res.isLeft) return res
    }
    unitR
  }

  def validateAllDiscard[A](
    in: mutable.Set[A]
  )(f: A => Either[ValidationError, Unit]): Either[ValidationError, Unit] = {
    val it = in.iterator
    while (it.hasNext) {
      val res = f(it.next())
      if (res.isLeft) return res
    }
    unitR
  }

  def validateAllNonEmpty[A](
    in: List[A]
  )(f: A => Either[ValidationError, Unit]): Option[Either[ValidationError, Unit]] =
    if (in.isEmpty) None
    else Some(validateAllDiscard(in)(f))

  def validateAll[A, B](
    in: List[A]
  )(f: A => Either[ValidationError, B]): Either[ValidationError, List[B]] = {
    var i       = 0
    var rem     = in
    var err     = null.asInstanceOf[ValidationError]
    val builder = ListBuffer.empty[B]
    while ((rem ne Nil) && (err eq null)) {
      val value = rem.head
      f(value) match {
        case Right(v) => builder.addOne(v)
        case Left(e)  => err = e
      }
      i += 1
      rem = rem.tail
    }
    if (err eq null) Right(builder.result()) else Left(err)
  }

  def failWhen(
    condition: Boolean
  )(msg: => String, explanatoryText: => String): Either[ValidationError, Unit] =
    if (condition) Validator.failValidation(msg, explanatoryText) else unitR

  final implicit class EitherOps[E, A](private val self: Either[E, A]) extends AnyVal {
    def *>[B](other: Either[E, B]): Either[E, B] =
      self match {
        case _: Right[?, ?]                      => other
        case l: Left[E @unchecked, B @unchecked] => l
      }

    def as[B](a: B): Either[E, B] =
      self match {
        case _: Right[?, ?]                      => Right(a)
        case l: Left[E @unchecked, B @unchecked] => l
      }
  }

  implicit class EnrichedListBufferOps[A](private val lb: ListBuffer[A]) extends AnyVal {
    // This method doesn't exist in Scala 2.12 so we just use `.map` for it instead
    def addOne(elem: A): ListBuffer[A]            = lb += elem
    def addAll(elems: Iterable[A]): ListBuffer[A] = lb ++= elems
  }
}
