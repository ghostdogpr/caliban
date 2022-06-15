package caliban.interop.cats

import cats.Applicative
import cats.data.{ Chain, Kleisli }
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.effect.{ IO, Ref }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.unsafe.implicits.global
import zio.{ Runtime, ZEnvironment, ZIO }
import zio.test._

object CatsInteropSpec extends ZIOSpecDefault {

  override def spec = suite("CatsInteropSpec")(
    test("contextual interop: inject an environment") {
      case class SecurityContext(isAdmin: Boolean)
      case class LogContext(traceId: String)
      case class RootContext(security: SecurityContext, log: LogContext)

      type Effect[A] = Kleisli[IO, RootContext, A]

      val rootCtx = RootContext(SecurityContext(false), LogContext("external-trace-id"))
      val inner   = RootContext(SecurityContext(true), LogContext("internal-trace-id"))

      def main(inner: RootContext)(implicit runtime: Runtime[RootContext]) =
        Dispatcher[Effect].use { dispatcher =>
          program[Effect, RootContext](CatsInterop.contextual(dispatcher), inner)
        }

      for {
        contextual <- ZIO.succeed(main(inner)(Runtime.default.as(ZEnvironment(rootCtx))).run(rootCtx).unsafeRunSync())
      } yield assertTrue(contextual == List(rootCtx, inner, rootCtx))
    },
    test("plain interop: do not inject an environment") {
      case class Context(traceId: String)

      type Effect[A] = Kleisli[IO, Context, A]

      val rootCtx = Context("external-trace-id")
      val inner   = Context("internal-trace-id")

      def main(inner: Context)(implicit runtime: Runtime[Context]) =
        Dispatcher[Effect].use { dispatcher =>
          program[Effect, Context](CatsInterop.default(dispatcher), inner)
        }

      for {
        contextual <- ZIO.succeed(main(inner)(Runtime.default.as(ZEnvironment(rootCtx))).run(rootCtx).unsafeRunSync())
      } yield assertTrue(contextual == List(rootCtx, rootCtx, rootCtx))
    }
  )

  private def program[F[_]: Async, R](interop: CatsInterop[F, R], inner: R)(implicit local: Local[F, R]): F[List[R]] = {

    def snapshot(ref: Ref[F, Chain[R]]): F[Unit] =
      for {
        current <- local.ask
        _       <- ref.update(_.append(current))
      } yield ()

    for {
      ref <- Ref.of(Chain.empty[R])
      _   <- snapshot(ref)
      _   <- local.local(interop.toEffect(interop.fromEffect(snapshot(ref))), inner)
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
