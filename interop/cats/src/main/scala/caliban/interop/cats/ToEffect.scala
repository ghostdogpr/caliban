package caliban.interop.cats

import cats.effect.Async
import cats.{ ~>, Monad }
import cats.syntax.flatMap._
import cats.syntax.functor._
import zio.{ RIO, Runtime }

/**
 * Describes how a polymorphic effect `F` can be created from [[zio.RIO]].
 *
 * @tparam F the higher-kinded type of a polymorphic effect
 * @tparam R the type of ZIO environment
 */
@annotation.implicitNotFound("""
Could not find `ToEffect` for effect ${F} and environment ${R}. `ToEffect` can be one of the following:

1) Non-contextual: derived automatically when `cats.effect.Async` is available in the implicit scope. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

implicit val asyncF: Async[${F}] = ???
implicit val toEffect: ToEffect[${F}, ${R}] = implicitly // or an explicit call `ToEffect.forAsync`

2) Contextual: injects ZIO environment into underlying effect. Can be used to share a context between ZIO and Kleisli-like effects:

case class Context(isAdmin: Boolean)
type Effect[A] = Kleisli[IO, Context, A]

val dispatcher: Dispatcher[Effect] = ???

implicit val injectContext: InjectEnv[Effect, Context] = InjectEnv.kleisli
implicit val toEffect: ToEffect[Effect, Context] = ToEffect.contextual

""")
trait ToEffect[F[_], R] {
  def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A]

  final def toEffectK(implicit runtime: Runtime[R]): RIO[R, *] ~> F =
    new (RIO[R, *] ~> F) {
      def apply[A](rio: RIO[R, A]): F[A] = toEffect(rio)
    }
}

object ToEffect {

  /**
   * Contextual version of the [[ToEffect]].
   *
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  trait Contextual[F[_], R] extends ToEffect[F, R] {
    def toEffect[A](rio: RIO[R, A], env: R)(implicit runtime: Runtime[R]): F[A]
  }

  def apply[F[_], R](implicit ev: ToEffect[F, R]): ToEffect[F, R] = ev

  /**
   * Contextual conversion of [[zio.RIO]] to a polymorphic effect `F`.
   *
   * An environment of type `R` can be injected into the effect `F` via `injector`.
   *
   * @see See [[InjectEnv]] for more details about injection.
   *
   * @param injector injects the given environment of type `R` into the effect `F`
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  def contextual[F[_]: Async, R](implicit injector: InjectEnv[F, R]): ToEffect.Contextual[F, R] =
    contextual(forAsync[F, R])

  /**
   * Contextual conversion of [[zio.RIO]] to a polymorphic effect `F`.
   *
   * An environment of type `R` can be injected into the effect `F` via `injector`.
   *
   * @see See [[InjectEnv]] for more details about injection.
   *
   * @param to the underlying conversion from [[zio.RIO]] to `F`
   * @param injector injects the given environment of type `R` into the effect `F`
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  def contextual[F[_]: Monad, R](to: ToEffect[F, R])(implicit injector: InjectEnv[F, R]): ToEffect.Contextual[F, R] =
    new ToEffect.Contextual[F, R] {
      def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A] =
        for {
          env    <- to.toEffect(RIO.environment[R])
          result <- toEffect(rio, env)
        } yield result

      def toEffect[A](rio: RIO[R, A], env: R)(implicit runtime: Runtime[R]): F[A] =
        injector.inject(to.toEffect(rio), env)
    }

  /**
   * Default (non-contextual) conversion of [[zio.RIO]] to a polymorphic effect `F`.
   *
   * Identical to what [[https://github.com/zio/interop-cats]] offers.
   *
   * @param F the instance of [[cats.effect.Async]]. Required in order to perform the conversion
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  implicit def forAsync[F[_], R](implicit F: Async[F]): ToEffect[F, R] =
    new ToEffect[F, R] {
      def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A] =
        zio.interop.toEffect(rio)
    }

}
