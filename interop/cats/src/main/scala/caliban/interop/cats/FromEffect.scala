package caliban.interop.cats

import cats.~>
import cats.effect.Async
import cats.effect.std.Dispatcher
import zio.{ RIO, Runtime }

trait ToEffect[F[_], R] {
  def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A]

  final def toEffectK(implicit runtime: Runtime[R]): RIO[R, *] ~> F =
    new (RIO[R, *] ~> F) {
      def apply[A](rio: RIO[R, A]): F[A] = toEffect(rio)
    }
}

object ToEffect {

  def apply[F[_], R](implicit ev: ToEffect[F, R]): ToEffect[F, R] = ev

  implicit def forAsync[F[_], R](implicit F: Async[F]): ToEffect[F, R] =
    new ToEffect[F, R] {
      def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A] =
        zio.interop.toEffect(rio)
    }

}

trait FromEffect[F[_], R] {
  def fromEffect[A](fa: F[A]): RIO[R, A]

  final lazy val fromEffectK: F ~> RIO[R, *] =
    new (F ~> RIO[R, *]) {
      def apply[A](fa: F[A]): RIO[R, A] = fromEffect(fa)
    }

}

object FromEffect {

  def apply[F[_], R](implicit ev: FromEffect[F, R]): FromEffect[F, R] = ev

  implicit def forDispatcher[F[_], R](implicit F: Dispatcher[F]): FromEffect[F, R] =
    new FromEffect[F, R] {
      def fromEffect[A](fa: F[A]): RIO[R, A] =
        zio.interop.fromEffect(fa)
    }

}
