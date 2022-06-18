package caliban.interop.cats

import cats.effect.Async
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ ~>, Monad }
import zio.{ RIO, Runtime, Tag, ZEnvironment, ZIO }

import scala.concurrent.Future

/**
 * Describes how a polymorphic effect `F` can be created from [[zio.RIO]].
 *
 * @tparam F the higher-kinded type of a polymorphic effect
 * @tparam R the type of ZIO environment
 */
@annotation.implicitNotFound("""
Could not find `ToEffect` for effect ${F} and environment ${R}. `ToEffect` can be one of the following:

1) Non-contextual: derived automatically when `cats.effect.Async` and `Runtime` are available in the implicit scope. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

implicit val async: Async[${F}] = ???
implicit val runtime: Runtime[${R}] = ???
implicit val toEffect: ToEffect[${F}, ${R}] = implicitly // or an explicit call `ToEffect.forAsync`

2) Contextual: injects ZIO environment into underlying effect. Can be used to share a context between ZIO and Kleisli-like effects:

case class Context(isAdmin: Boolean)
type Effect[A] = Kleisli[IO, Context, A]

val dispatcher: Dispatcher[Effect] = ???

implicit val runtime: Runtime[Context] = ???
implicit val injectContext: InjectEnv[Effect, Context] = InjectEnv.kleisli
implicit val toEffect: ToEffect[Effect, Context] = ToEffect.contextual

""")
trait ToEffect[F[_], R] {
  def toEffect[A](rio: RIO[R, A]): F[A]

  final val toEffectK: RIO[R, *] ~> F =
    new (RIO[R, *] ~> F) {
      def apply[A](rio: RIO[R, A]): F[A] = toEffect(rio)
    }
}

/**
 * @define contextualConversion
 *         Contextual conversion from [[zio.RIO]] to a polymorphic effect `F`.
 *
 *         An environment of type `R` is injected into the effect `F` via `injector`.
 *         The execution of `RIO[R, A]` relies on the environment `R` taken from the parent `F` context via `askEnv`.
 *
 *         @see See [[InjectEnv]] for more details about injection.
 *
 * @define injectorParam injects the given environment of type `R` into the effect `F`
 *
 * @define fParam the higher-kinded type of a polymorphic effect
 *
 * @define rParam the type of ZIO environment
 */
object ToEffect {

  /**
   * Contextual version of the [[ToEffect]].
   *
   * @tparam F $fParam
   * @tparam R $rParam
   */
  trait Contextual[F[_], R] extends ToEffect[F, R] {
    def toEffect[A](rio: RIO[R, A], env: R): F[A]
  }

  def apply[F[_], R](implicit ev: ToEffect[F, R]): ToEffect[F, R] = ev

  /**
   * $contextualConversion
   *
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_]: Async, R: Tag](implicit
    injector: InjectEnv[F, R],
    runtime: Runtime[R]
  ): ToEffect.Contextual[F, R] =
    contextual(forAsync[F, R])

  /**
   * $contextualConversion
   *
   * @param to the underlying conversion from [[zio.RIO]] to `F`
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_]: Monad, R: Tag](
    to: ToEffect[F, R]
  )(implicit injector: InjectEnv[F, R]): ToEffect.Contextual[F, R] =
    new ToEffect.Contextual[F, R] {
      def toEffect[A](rio: RIO[R, A]): F[A] =
        for {
          rEnv   <- to.toEffect(ZIO.environment[R])
          env    <- injector.modify(rEnv.get)
          result <- toEffect(rio, env)
        } yield result

      def toEffect[A](rio: RIO[R, A], env: R): F[A] =
        injector.inject(to.toEffect(rio.provideSomeEnvironment(_ => ZEnvironment(env))), env)
    }

  /**
   * Default (non-contextual) conversion from [[zio.RIO]] to a polymorphic effect `F`.
   *
   * Identical to what [[https://github.com/zio/interop-cats]] offers.
   *
   * @param F the instance of [[cats.effect.Async]]. Required in order to perform the conversion
   * @param runtime the instance of `zio.Runtime`. Required in order to perform the conversion
   * @tparam F $fParam
   * @tparam R $rParam
   */
  implicit def forAsync[F[_], R](implicit F: Async[F], runtime: Runtime[R]): ToEffect[F, R] =
    new ToEffect[F, R] {
      def toEffect[A](rio: RIO[R, A]): F[A] =
        F.uncancelable { poll =>
          F.delay(runtime.unsafeRunToFuture(rio)).flatMap { future =>
            poll(F.onCancel(F.fromFuture(F.pure[Future[A]](future)), F.fromFuture(F.delay(future.cancel())).void))
          }
        }
    }

}
