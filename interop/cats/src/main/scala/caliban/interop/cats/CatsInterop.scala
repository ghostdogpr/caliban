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
import zio.{ Runtime, _ }
import zio.query.ZQuery

import scala.concurrent.Future

object CatsInterop {

  def executeAsync[F[_]: Async, R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    extensions: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit runtime: Runtime[R]): F[GraphQLResponse[E]] = {
    val execution = graphQL.execute(
      query,
      operationName,
      variables,
      extensions,
      skipValidation = skipValidation,
      enableIntrospection = enableIntrospection,
      queryExecution
    )

    toEffect(execution)
  }

  def checkAsync[F[_]: Async, R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit runtime: Runtime[Any]): F[Unit] =
    toEffect(graphQL.check(query))

  def interpreterAsync[F[_]: Async, R](
    graphQL: GraphQL[R]
  )(implicit runtime: Runtime[Any]): F[GraphQLInterpreter[R, CalibanError]] =
    toEffect(graphQL.interpreter)

  def schema[F[_], R, A](implicit F: Dispatcher[F], ev: Schema[R, A]): Schema[R, F[A]] =
    new Schema[R, F[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def optional: Boolean =
        ev.optional

      override def resolve(value: F[A]): Step[R] =
        QueryStep(ZQuery.fromZIO(fromEffect(value).map(ev.resolve)))
    }

  def fromEffect[F[_], A](fa: F[A])(implicit F: Dispatcher[F]): Task[A] =
    ZIO
      .succeed(F.unsafeToFutureCancelable(fa))
      .flatMap { case (future, cancel) =>
        ZIO.fromFuture(_ => future).onInterrupt(ZIO.fromFuture(_ => cancel()).orDie).interruptible
      }
      .uninterruptible

  def toEffect[F[_], R, A](rio: RIO[R, A])(implicit F: Async[F], R: Runtime[R]): F[A] =
    F.uncancelable { poll =>
      F.delay(R.unsafeRunToFuture(rio)).flatMap { future =>
        poll(F.onCancel(F.fromFuture(F.pure[Future[A]](future)), F.fromFuture(F.delay(future.cancel())).void))
      }
    }

  def fromEffectK[F[_], R](implicit F: Dispatcher[F]): F ~> RIO[R, *] =
    new (F ~> RIO[R, *]) {
      def apply[A](fa: F[A]): RIO[R, A] = fromEffect(fa)
    }

  def toEffectK[F[_], R](implicit F: Async[F], R: Runtime[R]): RIO[R, *] ~> F =
    new (RIO[R, *] ~> F) {
      def apply[A](rio: RIO[R, A]): F[A] = toEffect(rio)
    }

}
