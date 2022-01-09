package caliban.interop.cats

import caliban.execution.QueryExecution
import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ Schema, Step }
import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.~>
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import cats.syntax.functor._
import zio.{ RIO, Runtime, Task, ZIO }
import zio.query.ZQuery

@annotation.implicitNotFound("""
Could not find `CatsInterop` for effect ${F} and environment ${R}. `CatsInterop` can be one of the following:

1) Plain: default conversion between zio and cats-effect. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

val dispatcher: Dispatcher[IO] = ???

implicit def catsInterop[R]: CatsInterop[IO, R] = CatsInterop.plain(dispatcher)

2) Contextual: injects ZIO environment into underlying effect. Can be used to share a context between ZIO and Kleisli-like effects:

case class Context(isAdmin: Boolean)
type Effect[A] = Kleisli[IO, Context, A]

val dispatcher: Dispatcher[Effect] = ???

implicit val injectContext: InjectEnv[Effect, Context] = InjectEnv.kleisli
implicit val catsInterop: CatsInterop[Effect, Context] = CatsInterop.contextual(dispatcher)

""")
trait CatsInterop[F[_], R] extends FromEffect[F, R] with ToEffect[F, R]

object CatsInterop {

  def apply[F[_], R](implicit ev: CatsInterop[F, R]): CatsInterop[F, R] = ev

  /**
   * A contextual interop between cats-effect and ZIO.
   *
   * An environment of type `R` can be injected into the effect `F` via `injector`.
   *
   * @see See [[InjectEnv]] for more details about injection.
   *
   * @param dispatcher evaluates the given effect `F` as `scala.concurrent.Future`
   * @param injector injects the given environment of type `R` into the effect `F`
   * @tparam F the higher-kinded type of an effect
   * @tparam R the type of the environment
   */
  def contextual[F[_]: Async, R](dispatcher: Dispatcher[F])(implicit injector: InjectEnv[F, R]): CatsInterop[F, R] =
    contextual(CatsInterop.default[F, R](dispatcher))

  /**
   * A contextual interop between cats-effect and ZIO.
   *
   * An environment of type `R` can be injected into the effect `F` via `injector`.
   *
   * @see See [[InjectEnv]] for more details about injection.
   *
   * @param underlying the parent interop
   * @param injector injects the given environment of type `R` into the effect `F`
   * @tparam F the higher-kinded type of an effect
   * @tparam R the type of the environment
   */
  def contextual[F[_]: Async, R](underlying: CatsInterop[F, R])(implicit injector: InjectEnv[F, R]): CatsInterop[F, R] =
    new CatsInterop[F, R] {
      def fromEffect[A](fa: F[A]): RIO[R, A] =
        for {
          env    <- ZIO.environment[R]
          result <- underlying.fromEffect(injector.inject(fa, env))
        } yield result

      def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A] =
        for {
          env    <- underlying.toEffect(ZIO.environment[R])
          result <- injector.inject(underlying.toEffect(rio), env)
        } yield result
    }

  /**
   * A default interop between cats-effect and ZIO.
   * Identical to what [[https://github.com/zio/interop-cats]] offers.
   *
   * @see See [[ToEffect.forAsync]] and [[FromEffect.forDispatcher]] for more details.
   *
   * @param dispatcher evaluates the given effect `F` as `scala.concurrent.Future`
   * @tparam F the higher-kinded type of an effect
   * @tparam R the type of the environment
   */
  def default[F[_], R](dispatcher: Dispatcher[F])(implicit F: Async[F]): CatsInterop[F, R] =
    make(ToEffect.forAsync[F, R], FromEffect.forDispatcher(dispatcher))

  def make[F[_], R](to: ToEffect[F, R], from: FromEffect[F, R]): CatsInterop[F, R] =
    new CatsInterop[F, R] {
      def fromEffect[A](fa: F[A]): RIO[R, A] =
        from.fromEffect(fa)

      def toEffect[A](rio: RIO[R, A])(implicit runtime: Runtime[R]): F[A] =
        to.toEffect(rio)
    }

  implicit def materialize[F[_], R](implicit
    toEffect: ToEffect[F, R],
    fromEffect: FromEffect[F, R]
  ): CatsInterop[F, R] =
    make(toEffect, fromEffect)

  // Utility methods

  def executeAsync[F[_], R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map.empty,
    extensions: Map[String, InputValue] = Map.empty,
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit interop: ToEffect[F, R], runtime: Runtime[R]): F[GraphQLResponse[E]] = {
    val execution = graphQL.execute(
      query,
      operationName,
      variables,
      extensions,
      skipValidation = skipValidation,
      enableIntrospection = enableIntrospection,
      queryExecution
    )

    interop.toEffect(execution)
  }

  def checkAsync[F[_], R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit interop: ToEffect[F, Any], runtime: Runtime[Any]): F[Unit] =
    interop.toEffect(graphQL.check(query))

  def interpreterAsync[F[_], R](
    graphQL: GraphQL[R]
  )(implicit interop: ToEffect[F, Any], runtime: Runtime[Any]): F[GraphQLInterpreter[R, CalibanError]] =
    interop.toEffect(graphQL.interpreter)

  def schema[F[_], R, A](implicit interop: FromEffect[F, R], ev: Schema[R, A]): Schema[R, F[A]] =
    new Schema[R, F[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def optional: Boolean =
        ev.optional

      override def resolve(value: F[A]): Step[R] =
        QueryStep(ZQuery.fromEffect(interop.fromEffect(value).map(ev.resolve)))
    }

  @deprecated("Use `CatsInterop[F, Any].fromEffect` or `FromEffect[F, Any].fromEffect`", "1.4.0")
  def fromEffect[F[_], A](fa: F[A])(implicit F: Dispatcher[F]): Task[A] =
    FromEffect.forDispatcher[F, Any].fromEffect(fa)

  @deprecated("Use `CatsInterop[F, R].toEffect` or `ToEffect[F, R].toEffect`", "1.4.0")
  def toEffect[F[_], R, A](rio: RIO[R, A])(implicit F: Async[F], R: Runtime[R]): F[A] =
    ToEffect.forAsync[F, R].toEffect(rio)

  @deprecated("Use `CatsInterop[F, R].fromEffectK` or `FromEffect[F, R].fromEffectK`", "1.4.0")
  def fromEffectK[F[_], R](implicit F: Dispatcher[F]): F ~> RIO[R, *] =
    FromEffect.forDispatcher[F, R].fromEffectK

  @deprecated("Use `CatsInterop[F, R].toEffectK` or `FromEffect[F, R].fromEffectK`", "1.4.0")
  def toEffectK[F[_], R](implicit F: Async[F], R: Runtime[R]): RIO[R, *] ~> F =
    ToEffect.forAsync[F, R].toEffectK

}
