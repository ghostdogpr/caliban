package caliban.interop.cats

import cats.data.Kleisli
import cats.effect.IO

/**
 * Injects a given environment of type `R` into the effect `F`.
 *
 * @tparam F the higher-kinded type of an effect
 * @tparam R the type of the environment
 */
@annotation.implicitNotFound("""
Could not find `InjectEnv` for effect ${F} and environment ${R}. `InjectEnv` can be one of the following:

1) Ignore: injects nothing into the effect. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

implicit def injectNothing[R]: InjectEnv[IO, R] = InjectEnv.ignore

2) Kleisli: injects given environment into the underlying effect in Kleisli:

case class Context(isAdmin: Boolean)
type Eff[A] = Kleisli[IO, Context, A]

implicit val injectContext: InjectEnv[Eff, A] = InjectEnv.kleisli

3) Kleisli + lens: injects a zoomed value (via lens) of the given environment into the underlying effect in Kleisli:

case class Security(isAdmin: Boolean)
case class Context(security: Security, lastAccess: Instant)

type Eff[A] = Kleisli[IO, Security, A]

implicit val injectSecurity: InjectCtx[Eff, Context] = InjectCtx.kleisliLens(_.security)

""")
trait InjectEnv[F[_], -R] {
  def inject[A](fa: F[A], env: R): F[A]
}

object InjectEnv {

  /**
   * Ignores a given environment
   */
  def ignore[F[_], R]: InjectEnv[F, R] =
    new InjectEnv[F, R] {
      def inject[A](fa: F[A], env: R): F[A] = fa
    }

  /**
   * Injects a zoomed value (via lens) of the given environment into the underlying effect in Kleisli.
   *
   * @param lens zooms `R1` inside of the `R`
   * @tparam F the higher-kinded type of an effect
   * @tparam R the type of the environment
   * @tparam R1 the zoomed typed inside of the `R`
   * @return
   */
  def kleisliLens[F[_], R, R1](lens: R => R1): InjectEnv[Kleisli[F, R1, *], R] =
    new InjectEnv[Kleisli[F, R1, *], R] {
      def inject[A](fa: Kleisli[F, R1, A], env: R): Kleisli[F, R1, A] =
        Kleisli.liftF(fa.run(lens(env)))
    }

  /**
   * Injects a given environment into the underlying effect in Kleisli:
   *
   * @tparam F the higher-kinded type of an effect
   * @tparam R the type of the environment
   */
  def kleisli[F[_], R]: InjectEnv[Kleisli[F, R, *], R] =
    new InjectEnv[Kleisli[F, R, *], R] {
      def inject[A](fa: Kleisli[F, R, A], env: R): Kleisli[F, R, A] =
        Kleisli.liftF(fa.run(env))
    }

  implicit def injectEnvIO[R]: InjectEnv[IO, R] = InjectEnv.ignore

  implicit def injectEnvKleisli[F[_], R]: InjectEnv[Kleisli[F, R, *], R] = InjectEnv.kleisli[F, R]

}
