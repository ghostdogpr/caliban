package caliban.relay

import zio.NonEmptyChunk
import caliban.relay.Validation._

private[caliban] sealed trait Validation[+E, +A] { self =>
  final def <&>[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    zipPar(that)

  final def flatMap[B, E1 >: E](f: A => Validation[E1, B]): Validation[E1, B] =
    self match {
      case e @ Failure(_) => e
      case Success(v)     => f(v)
    }

  final def map[B](f: A => B): Validation[E, B] =
    self match {
      case e @ Failure(_) => e
      case Success(v)     => Success(f(v))
    }

  final def fold[B](failure: NonEmptyChunk[E] => B, success: A => B): B =
    self match {
      case Failure(errors) => failure(errors)
      case Success(value)  => success(value)
    }

  final def zipPar[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    zipWithPar(that)((_, _))

  final def zipWithPar[E1 >: E, B, C](that: Validation[E1, B])(f: (A, B) => C): Validation[E1, C] =
    (self, that) match {
      case (Failure(e), Failure(e1)) => Failure(e ++ e1)
      case (Failure(e), Success(_))  => Failure(e)
      case (Success(_), Failure(e1)) => Failure(e1)
      case (Success(a), Success(b))  => Success(f(a, b))
    }
}

private[caliban] object Validation {
  final case class Failure[+E](errors: NonEmptyChunk[E]) extends Validation[E, Nothing]
  final case class Success[+A](value: A)                 extends Validation[Nothing, A]

  def fail[E](error: E): Validation[E, Nothing] =
    Failure(NonEmptyChunk(error))

  def succeed[A](value: A): Validation[Nothing, A] =
    Success(value)

  def fromEither[E, A](value: Either[E, A]): Validation[E, A] =
    value.fold(fail, succeed)

  def validateWith[E, A0, A1, B](a0: Validation[E, A0], a1: Validation[E, A1])(
    f: (A0, A1) => B
  ): Validation[E, B] =
    a0.zipWithPar(a1)(f)

  def validateWith[E, A0, A1, A2, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2]
  )(
    f: (A0, A1, A2) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2).map { case ((a0, a1), a2) => f(a0, a1, a2) }

  def validateWith[E, A0, A1, A2, A3, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3).map { case (((a0, a1), a2), a3) => f(a0, a1, a2, a3) }

  def validateWith[E, A0, A1, A2, A3, A4, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4).map { case ((((a0, a1), a2), a3), a4) => f(a0, a1, a2, a3, a4) }

  def validateWith[E, A0, A1, A2, A3, A4, A5, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5).map { case (((((a0, a1), a2), a3), a4), a5) => f(a0, a1, a2, a3, a4, a5) }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6).map { case ((((((a0, a1), a2), a3), a4), a5), a6) =>
      f(a0, a1, a2, a3, a4, a5, a6)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7).map { case (((((((a0, a1), a2), a3), a4), a5), a6), a7) =>
      f(a0, a1, a2, a3, a4, a5, a6, a7)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8).map {
      case ((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9).map {
      case (((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10).map {
      case ((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11).map {
      case (((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12).map {
      case ((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13).map {
      case (((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14).map {
      case ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15).map {
      case (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16).map {
      case ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17).map {
      case (
            ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
            a17
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18).map {
      case (
            (
              ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
              a17
            ),
            a18
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    }

  def validateWith[E, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19).map {
      case (
            (
              (
                (
                  (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                  a16
                ),
                a17
              ),
              a18
            ),
            a19
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    }

  def validateWith[
    W,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    B
  ](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20).map {
      case (
            (
              (
                (
                  (
                    (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                    a16
                  ),
                  a17
                ),
                a18
              ),
              a19
            ),
            a20
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    }

  def validateWith[
    W,
    E,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    B
  ](
    a0: Validation[E, A0],
    a1: Validation[E, A1],
    a2: Validation[E, A2],
    a3: Validation[E, A3],
    a4: Validation[E, A4],
    a5: Validation[E, A5],
    a6: Validation[E, A6],
    a7: Validation[E, A7],
    a8: Validation[E, A8],
    a9: Validation[E, A9],
    a10: Validation[E, A10],
    a11: Validation[E, A11],
    a12: Validation[E, A12],
    a13: Validation[E, A13],
    a14: Validation[E, A14],
    a15: Validation[E, A15],
    a16: Validation[E, A16],
    a17: Validation[E, A17],
    a18: Validation[E, A18],
    a19: Validation[E, A19],
    a20: Validation[E, A20],
    a21: Validation[E, A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): Validation[E, B] =
    (a0 <&> a1 <&> a2 <&> a3 <&> a4 <&> a5 <&> a6 <&> a7 <&> a8 <&> a9 <&> a10 <&> a11 <&> a12 <&> a13 <&> a14 <&> a15 <&> a16 <&> a17 <&> a18 <&> a19 <&> a20 <&> a21).map {
      case (
            (
              (
                (
                  (
                    (
                      (
                        ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14),
                        a15
                      ),
                      a16
                    ),
                    a17
                  ),
                  a18
                ),
                a19
              ),
              a20
            ),
            a21
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
    }
}
