package caliban.interop.monix

import caliban.execution.QueryExecution
import caliban.introspection.adt.__Type
import caliban.schema.Step.{ QueryStep, StreamStep }
import caliban.schema.{ Schema, Step, Types }
import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.effect.ConcurrentEffect
import monix.eval.{ Task => MonixTask }
import monix.reactive.Observable
import zio._
import zio.interop.catz._
import zio.interop.reactivestreams._
import zio.stream.ZStream
import zio.query.ZQuery

object MonixInterop {

  def executeAsync[R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    extensions: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit runtime: Runtime[R]): MonixTask[GraphQLResponse[E]] =
    MonixTask.async { cb =>
      val execution = graphQL.execute(
        query,
        operationName,
        variables,
        extensions,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution
      )
      Unsafe.unsafe(implicit u => MonixTask.deferFuture(runtime.unsafe.runToFuture(execution)))
    }

  def checkAsync[R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit runtime: Runtime[Any]): MonixTask[Unit] =
    MonixTask.async(cb =>
      Unsafe.unsafe(implicit u => MonixTask.deferFuture(runtime.unsafe.runToFuture(graphQL.check(query))))
    )

  def interpreterAsync[R](
    graphQL: GraphQL[R]
  )(implicit runtime: Runtime[Any]): MonixTask[GraphQLInterpreter[R, CalibanError]] =
    MonixTask.async(cb =>
      Unsafe.unsafe(implicit u => MonixTask.deferFuture(runtime.unsafe.runToFuture(graphQL.interpreter)))
    )

  def taskSchema[R, A](implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, MonixTask[A]] =
    new Schema[R, MonixTask[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def optional: Boolean                                         = ev.optional
      override def resolve(value: MonixTask[A]): Step[R]                     =
        QueryStep(ZQuery.fromZIO(value.to[Task].map(ev.resolve)))
    }

  def observableSchema[R, A](
    queueSize: Int
  )(implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, Observable[A]] =
    new Schema[R, Observable[A]] {
      override def optional: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else (if (ev.optional) t else t.nonNull).list
      }
      override def resolve(value: Observable[A]): Step[R]                    =
        StreamStep(
          ZStream
            .fromZIO(
              MonixTask
                .deferAction(implicit sc =>
                  MonixTask.eval(value.toReactivePublisher.toZIOStream(queueSize).map(ev.resolve))
                )
                .to[Task]
            )
            .flatten
        )
    }
}
