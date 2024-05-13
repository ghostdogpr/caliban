package caliban.interop.cats

import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ Schema, Step }
import caliban._
import cats.{ ApplicativeThrow, Monad }
import cats.effect.Async
import cats.effect.std.Dispatcher
import zio.query.ZQuery
import zio.{ RIO, Runtime, Tag, ZEnvironment }

/**
 * Interop between `F` and [[zio.RIO]]. The combination of [[ToEffect]] and [[FromEffect]].
 *
 * Describes how [[zio.RIO]] can be created from a polymorphic effect `F`.
 * Describes how a polymorphic effect `F` can be created from [[zio.RIO]].
 *
 * @tparam F the higher-kinded type of a polymorphic effect
 * @tparam R the type of ZIO environment
 */
@annotation.implicitNotFound("""
Could not find `CatsInterop` for effect ${F} and environment ${R}. `CatsInterop` can be one of the following:

1) Non-contextual: default conversion between RIO and ${F}. A way to go for non-contextual effects (e.g. `cats.effect.IO`):

implicit val runtime: Runtime[${R}] = ???
val dispatcher: Dispatcher[${F}] = ???

implicit val catsInterop: CatsInterop[${F}, ${R}] = CatsInterop.default(dispatcher)

2) Contextual: injects ZIO environment into underlying effect. Can be used to share a context between ZIO and Kleisli-like effects:

case class Context(isAdmin: Boolean)
type Effect[A] = Kleisli[IO, Context, A]

val dispatcher: Dispatcher[Effect] = ???

implicit val runtime: Runtime[Context] = ???
implicit val injectContext: InjectEnv[Effect, Context] = InjectEnv.kleisli
implicit val catsInterop: CatsInterop[Effect, Context] = CatsInterop.contextual(dispatcher)

""")
trait CatsInterop[F[_], R] extends FromEffect[F, R] with ToEffect[F, R]

/**
 * @define contextualInterop
 *         Contextual interop between `F` and [[zio.RIO]].
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
 */
object CatsInterop {

  def apply[F[_], R](implicit ev: CatsInterop[F, R]): CatsInterop[F, R] = ev

  /**
   * Contextual version of the [[CatsInterop]].
   *
   * Inherits two utility methods from [[ToEffect.Contextual]] and [[FromEffect.Contextual]]:
   * {{{
   *   def toEffect[A](rio: RIO[R, A], env: R): F[A]
   *
   *   def fromEffect[A](fa: F[A], env: R): RIO[R, A]
   * }}}
   *
   * @tparam F $fParam
   * @tparam R $rParam
   */
  trait Contextual[F[_], R] extends CatsInterop[F, R] with FromEffect.Contextual[F, R] with ToEffect.Contextual[F, R]

  /**
   * $contextualInterop
   *
   * @param dispatcher $dispatcherParam
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_]: Async, R: Tag](dispatcher: Dispatcher[F])(implicit
    injector: InjectEnv[F, R],
    runtime: Runtime[R]
  ): Contextual[F, R] =
    contextual(CatsInterop.default[F, R](dispatcher))

  /**
   * $contextualInterop
   *
   * @param underlying the underlying interop between `F` and [[zio.RIO]]
   * @param injector $injectorParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def contextual[F[_]: Monad, R: Tag](
    underlying: CatsInterop[F, R]
  )(implicit injector: InjectEnv[F, R]): Contextual[F, R] =
    new CatsInterop.Contextual[F, R] {
      private val to   = ToEffect.contextual(underlying)
      private val from = FromEffect.contextual(underlying)

      def fromEffect[A](fa: F[A], env: ZEnvironment[R]): RIO[R, A] =
        from.fromEffect(fa, env)

      def toEffect[A](rio: RIO[R, A], env: R): F[A] =
        to.toEffect(rio, env)

      def toEffect[A](rio: RIO[R, A]): F[A] =
        to.toEffect(rio)
    }

  /**
   * Default (non-contextual) interop between `F` and [[zio.RIO]].
   * Identical to what [[https://github.com/zio/interop-cats]] offers.
   *
   * @see See [[ToEffect.forAsync]] and [[FromEffect.forDispatcher]] for more details.
   *
   * @param dispatcher $dispatcherParam
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def default[F[_]: Async, R](dispatcher: Dispatcher[F])(implicit runtime: Runtime[R]): CatsInterop[F, R] =
    make(ToEffect.forAsync[F, R], FromEffect.forDispatcher(dispatcher))

  /**
   * Creates an instance of [[CatsInterop]] using [[ToEffect]] and [[FromEffect]] under the hood.
   *
   * @param to the conversion from [[zio.RIO]] to `F`
   * @param from the conversion from `F` to [[zio.RIO]]
   * @tparam F $fParam
   * @tparam R $rParam
   */
  def make[F[_], R](to: ToEffect[F, R], from: FromEffect[F, R]): CatsInterop[F, R] =
    new CatsInterop[F, R] {
      def fromEffect[A](fa: F[A]): RIO[R, A] =
        from.fromEffect(fa)

      def toEffect[A](rio: RIO[R, A]): F[A] =
        to.toEffect(rio)
    }

  /**
   * Materializes an instance of [[CatsInterop]] using [[ToEffect]] and [[FromEffect]] available in the implicit scope.
   *
   * @param to the conversion from [[zio.RIO]] to `F`
   * @param from the conversion from `F` to [[zio.RIO]]
   * @tparam F $fParam
   * @tparam R $rParam
   */
  implicit def materialize[F[_], R](implicit to: ToEffect[F, R], from: FromEffect[F, R]): CatsInterop[F, R] =
    make(to, from)

  // Utility methods

  def executeAsync[F[_], R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map.empty,
    extensions: Map[String, InputValue] = Map.empty
  )(implicit interop: ToEffect[F, R]): F[GraphQLResponse[E]] = {
    val execution = graphQL.execute(query, operationName, variables, extensions)
    interop.toEffect(execution)
  }

  def checkAsync[F[_], R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit interop: ToEffect[F, Any]): F[Unit] =
    interop.toEffect(graphQL.check(query))

  @deprecated("use interpreterF instead")
  def interpreterAsync[F[_], R](
    graphQL: GraphQL[R]
  )(implicit interop: ToEffect[F, Any]): F[GraphQLInterpreter[R, CalibanError]] =
    interop.toEffect(graphQL.interpreter)

  def interpreterF[F[_]: ApplicativeThrow, R](graphQL: GraphQL[R]): F[GraphQLInterpreter[R, CalibanError]] =
    ApplicativeThrow[F].fromEither(graphQL.interpreterEither)

  def schema[F[_], R, A](implicit interop: FromEffect[F, R], ev: Schema[R, A]): Schema[R, F[A]] =
    new Schema[R, F[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def nullable: Boolean =
        ev.nullable
      override def canFail: Boolean  = true

      override def resolve(value: F[A]): Step[R] =
        QueryStep(ZQuery.fromZIO(interop.fromEffect(value).map(ev.resolve)))
    }
}
