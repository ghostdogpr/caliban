package caliban.interop.cats

import cats.Applicative
import cats.data.{ Chain, Kleisli }
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.effect.{ IO, Ref }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.unsafe.implicits.global
import zio.{ Runtime, ZEnv, ZIO }
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object CatsInteropSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] = suite("CatsInteropSpec")(
    testM("contextual interop: inject an environment") {
      case class SecurityContext(isAdmin: Boolean)
      case class LogContext(traceId: String)
      case class RootContext(security: SecurityContext, log: LogContext)

      type Effect[A] = Kleisli[IO, RootContext, A]

      val rootCtx = RootContext(SecurityContext(false), LogContext("external-trace-id"))
      val inner   = RootContext(SecurityContext(true), LogContext("internal-trace-id"))

      def main(inner: RootContext)(implicit runtime: Runtime[ZEnv]) =
        Dispatcher[Effect].use { dispatcher =>
          program[Effect, RootContext](CatsInterop.contextual(dispatcher), inner)
        }

      for {
        contextual <- ZIO.effectTotal(main(inner)(Runtime.default).run(rootCtx).unsafeRunSync())
      } yield assert(contextual)(equalTo(List(rootCtx, inner, rootCtx)))
    },
    testM("plain interop: do not inject an environment") {
      case class Context(traceId: String)

      type Effect[A] = Kleisli[IO, Context, A]

      val rootCtx = Context("external-trace-id")
      val inner   = Context("internal-trace-id")

      def main(inner: Context)(implicit runtime: Runtime[ZEnv]) =
        Dispatcher[Effect].use { dispatcher =>
          program[Effect, Context](CatsInterop.default(dispatcher), inner)
        }

      for {
        contextual <- ZIO.effectTotal(main(inner)(Runtime.default).run(rootCtx).unsafeRunSync())
      } yield assert(contextual)(equalTo(List(rootCtx, rootCtx, rootCtx)))
    }
  )

  private def program[F[_]: Async, R](interop: CatsInterop[F, R], inner: R)(implicit
    local: Local[F, R],
    runtime: Runtime[ZEnv]
  ): F[List[R]] = {

    def snapshot(ref: Ref[F, Chain[R]]): F[Unit] =
      for {
        current <- local.ask
        _       <- ref.update(_.append(current))
      } yield ()

    for {
      ref <- Ref.of(Chain.empty[R])
      _   <- snapshot(ref)
      _   <- local.local(interop.toEffect(interop.fromEffect(snapshot(ref)))(runtime.as(inner)), inner)
      _   <- snapshot(ref)
      r   <- ref.get
    } yield r.toList
  }

  trait Local[F[_], R] {
    def ask: F[R]
    def local[A](fa: F[A], r: R): F[A]
  }

  object Local {
    implicit def localForKleisli[F[_]: Applicative, R]: Local[Kleisli[F, R, *], R] =
      new Local[Kleisli[F, R, *], R] {
        def ask: Kleisli[F, R, R]                                  = Kleisli.ask
        def local[A](fa: Kleisli[F, R, A], r: R): Kleisli[F, R, A] = Kleisli.liftF(fa.run(r))
      }
  }

}
