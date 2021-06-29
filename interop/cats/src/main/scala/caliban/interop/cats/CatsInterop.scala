package caliban.interop.cats

import caliban.execution.QueryExecution
import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ Schema, Step }
import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.effect.implicits._
import cats.effect.{ Async, Effect }
import zio.interop.catz._
import zio.{ Runtime, _ }
import zio.query.ZQuery

object CatsInterop {

  def executeAsync[F[_]: Async, R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    extensions: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit runtime: Runtime[R]): F[GraphQLResponse[E]] =
    Async[F].async { cb =>
      val execution = graphQL.execute(
        query,
        operationName,
        variables,
        extensions,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )

      runtime.unsafeRunAsync(execution)(exit => cb(exit.toEither))
    }

  def checkAsync[F[_]: Async, R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit runtime: Runtime[Any]): F[Unit] =
    Async[F].async(cb => runtime.unsafeRunAsync(graphQL.check(query))(exit => cb(exit.toEither)))

  def interpreterAsync[F[_]: Async, R](
    graphQL: GraphQL[R]
  )(implicit runtime: Runtime[Any]): F[GraphQLInterpreter[R, CalibanError]] =
    Async[F].async(cb => runtime.unsafeRunAsync(graphQL.interpreter)(exit => cb(exit.toEither)))

  def schema[F[_]: Effect, R, A](implicit ev: Schema[R, A]): Schema[R, F[A]] =
    new Schema[R, F[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        ev.toType_(isInput, isSubscription)

      override def optional: Boolean =
        ev.optional

      override def resolve(value: F[A]): Step[R] =
        QueryStep(ZQuery.fromEffect(value.toIO.to[Task].map(ev.resolve)))
    }
}
