package caliban.interop.cats

import cats.Applicative
import cats.data.{ EitherT, Kleisli, OptionT }

/**
 * Injects a given environment of type `R` into the effect `F`.
 *
 * @tparam F the higher-kinded type of a polymorphic effect
 * @tparam R the type of ZIO environment
 */
@annotation.implicitNotFound("""
Could not find `InjectEnv` for effect ${F} and environment ${R}. `InjectEnv` can be one of the following:

1) Kleisli: injects given environment into the underlying effect in cats.data.Kleisli:

case class Context(isAdmin: Boolean)
type Eff[A] = Kleisli[IO, Context, A]

implicit val injectContext: InjectEnv[Eff, Context] = InjectEnv.kleisli

2) Kleisli + lens: injects a zoomed value (via lens) of the given environment into the underlying effect in Kleisli:

case class Security(isAdmin: Boolean)
case class Context(security: Security, lastAccess: Instant)

type Eff[A] = Kleisli[IO, Security, A]

implicit val injectSecurity: InjectEnv[Eff, Context] =
  InjectEnv.kleisliLens(_.security, (ctx, security) => ctx.copy(security = security))

""")
trait InjectEnv[F[_], R] {

  /**
   * Injects the given environment `R` into the effect `F`.
   */
  def inject[A](fa: F[A], env: R): F[A]

  /**
   * Modifies [[zio.RIO]] environment before re-injecting it into [[zio.RIO]] and `F`.
   *
   * @see See [[ToEffect]] for a particular usage.
   *
   * @param env an environment taken from [[zio.RIO]]
   */
  def modify(env: R): F[R]

}

object InjectEnv {

  /**
   * Injects a zoomed value (via lens) from the given environment into the underlying effect in [[cats.data.Kleisli]].
   *
   * Constructs a new environment using [[zio.RIO]] env and local context from [[cats.data.Kleisli]].
   *
   * Useful when [[zio.RIO]] and effect `F` are using different contextual environments.
   *
   * @param lens zooms `R1` inside of the `R`
   * @param mod the modification function that returns a new environment
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   * @tparam R1 the zoomed typed inside of the `R`
   * @return
   */
  def kleisliLens[F[_]: Applicative, R, R1](lens: R => R1, mod: (R, R1) => R): InjectEnv[Kleisli[F, R1, *], R] =
    new InjectEnv[Kleisli[F, R1, *], R] {
      def inject[A](fa: Kleisli[F, R1, A], env: R): Kleisli[F, R1, A] =
        Kleisli.liftF(fa.run(lens(env)))

      def modify(env: R): Kleisli[F, R1, R] =
        Kleisli.ask[F, R1].map(local => mod(env, local))
    }

  /**
   * Injects a given environment into the underlying effect in [[cats.data.Kleisli]].
   * Prioritizes Kleisli context over ZIO environment.
   *
   * @tparam F the higher-kinded type of a polymorphic effect
   * @tparam R the type of ZIO environment
   */
  def kleisli[F[_]: Applicative, R]: InjectEnv[Kleisli[F, R, *], R] =
    new InjectEnv[Kleisli[F, R, *], R] {
      def inject[A](fa: Kleisli[F, R, A], env: R): Kleisli[F, R, A] =
        Kleisli.liftF(fa.run(env))

      def modify(env: R): Kleisli[F, R, R] =
        Kleisli.ask[F, R]
    }

  implicit def injectEnvForKleisli[F[_]: Applicative, R]: InjectEnv[Kleisli[F, R, *], R] =
    InjectEnv.kleisli[F, R]

  implicit def injectEnvForOptionT[F[_]: Applicative, R](implicit
    injector: InjectEnv[F, R]
  ): InjectEnv[OptionT[F, *], R] =
    new InjectEnv[OptionT[F, *], R] {
      def inject[A](fa: OptionT[F, A], env: R): OptionT[F, A] =
        OptionT(injector.inject(fa.value, env))

      def modify(env: R): OptionT[F, R] =
        OptionT.pure[F](env)
    }

  implicit def injectEnvForEitherT[F[_]: Applicative, E, R](implicit
    injector: InjectEnv[F, R]
  ): InjectEnv[EitherT[F, E, *], R] =
    new InjectEnv[EitherT[F, E, *], R] {
      def inject[A](fa: EitherT[F, E, A], env: R): EitherT[F, E, A] =
        EitherT(injector.inject(fa.value, env))

      def modify(env: R): EitherT[F, E, R] =
        EitherT.pure[F, E](env)
    }
}
