package caliban.interop.cats

import cats.Applicative
import cats.data.{ Chain, Kleisli }
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{ IO, IOLocal, Ref }
import cats.syntax.flatMap._
import cats.syntax.functor._
import zio.test._
import zio.{ Runtime, ZEnvironment, ZIO }

object CatsInteropSpec extends ZIOSpecDefault {

  override def spec = suite("CatsInteropSpec")(
    suite("contextual interop: inject an environment") {
      case class SecurityContext(isAdmin: Boolean)
      case class LogContext(traceId: String)
      case class RootContext(security: SecurityContext, log: LogContext)

      val rootCtx = RootContext(SecurityContext(false), LogContext("external-trace-id"))
      val inner   = RootContext(SecurityContext(true), LogContext("internal-trace-id"))

      def main[F[_]: Async](inner: RootContext)(implicit
        runtime: Runtime[RootContext],
        inj: InjectEnv[F, RootContext],
        local: Local[F, RootContext]
      ) =
        Dispatcher.parallel[F].use { dispatcher =>
          program[F, RootContext](CatsInterop.contextual(dispatcher), inner)
        }

      List(
        test("Kleisli") {
          type Effect[A] = Kleisli[IO, RootContext, A]
          implicit val rtm: Runtime[RootContext] = Runtime.default.withEnvironment(ZEnvironment(rootCtx))
          for {
            contextual <- ZIO.succeedBlocking(main[Effect](inner).run(rootCtx).unsafeRunSync())
          } yield assertTrue(contextual == List(rootCtx, inner, rootCtx))
        },
        test("IO") {
          implicit val rtm: Runtime[RootContext] = Runtime.default.withEnvironment(ZEnvironment(rootCtx))
          for {
            contextual <- ZIO.succeedBlocking {
                            IOLocal(rootCtx).flatMap(implicit local => main[IO](inner)).unsafeRunSync()
                          }
          } yield assertTrue(contextual == List(rootCtx, inner, rootCtx))
        }
      )

    },
    suite("plain interop: do not inject an environment") {
      case class Context(traceId: String)
      val rootCtx = Context("external-trace-id")
      val inner   = Context("internal-trace-id")

      def main[F[_]: Async](inner: Context)(implicit runtime: Runtime[Context], local: Local[F, Context]) =
        Dispatcher.parallel[F].use { dispatcher =>
          program[F, Context](CatsInterop.default(dispatcher), inner)
        }

      List(
        test("Kleisli") {
          type Effect[A] = Kleisli[IO, Context, A]
          implicit val rtm: Runtime[Context] = Runtime.default.withEnvironment(ZEnvironment(rootCtx))
          for {
            contextual <- ZIO.succeedBlocking(main[Effect](inner).run(rootCtx).unsafeRunSync())
          } yield assertTrue(contextual == List(rootCtx, rootCtx, rootCtx))
        },
        test("IO") {
          implicit val rtm: Runtime[Context] = Runtime.default.withEnvironment(ZEnvironment(rootCtx))
          for {
            contextual <- ZIO.succeedBlocking {
                            IOLocal(rootCtx).flatMap(implicit local => main[IO](inner)).unsafeRunSync()
                          }
          } yield assertTrue(contextual == List(rootCtx, rootCtx, rootCtx))
        }
      )
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

    implicit def localForIO[R](implicit ioLocal: IOLocal[R]): Local[IO, R] =
      new Local[IO, R] {
        def ask: IO[R]                       = ioLocal.get
        def local[A](fa: IO[A], r: R): IO[A] = ioLocal.getAndSet(r).flatMap(v => fa.guarantee(ioLocal.set(v)))
      }
  }

}
