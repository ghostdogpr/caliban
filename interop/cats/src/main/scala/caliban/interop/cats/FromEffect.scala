package caliban.interop.cats

import cats.~>
import cats.effect.std.Dispatcher
import zio.RIO

/**
 * Describes how [[zio.RIO]] can be created from a polymorphic effect `F`.
 *
 * @tparam F the higher-kinded type of a polymorphic effect
 * @tparam R the type of ZIO environment
 */
@annotation.implicitNotFound("""
Could not find `FromEffect` for effect ${F} and environment ${R}. `FromEffect` can be one of the following:

1) Non-contextual: derived automatically when `cats.effect.std.Dispatcher` is available in the implicit scope. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

implicit val dispatcher: Dispatcher[${F}] = ???

val fromEffect: FromEffect[${F}, ${R}] = implicitly // or an explicit call `FromEffect.forDispatcher`

2) Contextual: injects ZIO environment into underlying effect. Can be used to share a context between ZIO and Kleisli-like effects:

case class Context(isAdmin: Boolean)
type Effect[A] = Kleisli[IO, Context, A]

val dispatcher: Dispatcher[Effect] = ???

implicit val injectContext: InjectEnv[Effect, Context] = InjectEnv.kleisli
implicit val fromEffect: FromEffect[Effect, Context] = FromEffect.contextual(dispatcher)

""")
trait FromEffect[F[_], R] {
  def fromEffect[A](fa: F[A]): RIO[R, A]

  final lazy val fromEffectK: F ~> RIO[R, *] =
    new (F ~> RIO[R, *]) {
      def apply[A](fa: F[A]): RIO[R, A] = fromEffect(fa)
    }

}

/**
 * @define contextualConversion
 *         Contextual conversion from a polymorphic effect `F` to [[zio.RIO]].
 *
 *         An environment of type `R` is injected into the effect `F` via `injector`.
 *         The execution of `RIO[R, A]` relies on the environment `R` modified by [[InjectEnv.modify]].
 *
 *         @see See [[InjectEnv]] for more details about injection.
 *
 * @define dispatcherParam the instance of [[cats.effect.std.Dispatcher]]. Required in order to perform the conversion
 *
 * @define injectorParam injects the given environment of type `R` into the effect `F`
 *
 * @define fParam the higher-kinded type of a polymorphic effect
 *
 * @define rParam the type of ZIO environment
 *
 */
object FromEffect {

  /**
   * Contextual version of the [[FromEffect]].
   *
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  trait Contextual[F[_], R] extends FromEffect[F, R] {
    def fromEffect[A](fa: F[A], env: R): RIO[R, A]

    final def fromEffect[A](fa: F[A]): RIO[R, A] =
      for {
        env    <- RIO.environment[R]
        result <- fromEffect(fa, env)
      } yield result
  }

  def apply[F[_], R](implicit ev: FromEffect[F, R]): FromEffect[F, R] = ev

  /**
   * $contextualConversion
   *
   * @param dispatcher $dispatcherParam
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_], R](dispatcher: Dispatcher[F])(implicit injector: InjectEnv[F, R]): FromEffect.Contextual[F, R] =
    contextual(forDispatcher[F, R](dispatcher))

  /**
   * $contextualConversion
   *
   * @param from the underlying conversion from `F` to [[zio.RIO]]
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_], R](from: FromEffect[F, R])(implicit injector: InjectEnv[F, R]): FromEffect.Contextual[F, R] =
    new FromEffect.Contextual[F, R] {
      def fromEffect[A](fa: F[A], env: R): RIO[R, A] =
        from.fromEffect(injector.inject(fa, env))
    }

  /**
   * Default (non-contextual) conversion from a polymorphic effect `F` to [[zio.RIO]].
   *
   * Identical to what [[https://github.com/zio/interop-cats]] offers.
   *
   * @param dispatcher $dispatcherParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  implicit def forDispatcher[F[_], R](implicit dispatcher: Dispatcher[F]): FromEffect[F, R] =
    new FromEffect[F, R] {
      def fromEffect[A](fa: F[A]): RIO[R, A] =
        zio.interop.fromEffect(fa)
    }

}
