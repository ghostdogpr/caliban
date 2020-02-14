package caliban.interop.monix

import caliban.introspection.adt.__Type
import caliban.schema.Step.{ QueryStep, StreamStep }
import caliban.schema.{ Schema, Step }
import caliban.{ GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.effect.ConcurrentEffect
import monix.eval.{ Task => MonixTask }
import monix.reactive.Observable
import zio._
import zio.interop.catz._
import zio.interop.reactiveStreams._
import zio.stream.ZStream
import zquery.ZQuery

object MonixInterop {

  def executeAsync[R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    skipValidation: Boolean = false
  )(implicit runtime: Runtime[R]): MonixTask[GraphQLResponse[E]] =
    MonixTask.async { cb =>
      val execution = graphQL.execute(query, operationName, variables, skipValidation)
      runtime.unsafeRunAsync(execution)(exit => cb(exit.toEither))
    }

  def checkAsync[R](graphQL: GraphQL[R])(query: String)(implicit runtime: Runtime[R]): MonixTask[Unit] =
    MonixTask.async { cb =>
      runtime.unsafeRunAsync(graphQL.check(query))(exit => cb(exit.toEither))
    }

  def taskSchema[R, A](implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, MonixTask[A]] =
    new Schema[R, MonixTask[A]] {
      override def toType(isInput: Boolean): __Type = ev.toType(isInput)
      override def optional: Boolean                = ev.optional
      override def resolve(value: MonixTask[A]): Step[R] =
        QueryStep(ZQuery.fromEffect(value.to[Task].map(ev.resolve)))
    }

  def observableSchema[R, A](
    queueSize: Int
  )(implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, Observable[A]] =
    new Schema[R, Observable[A]] {
      override def optional: Boolean                        = ev.optional
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: Observable[A]): Step[R] =
        StreamStep(
          ZStream.flatten(
            ZStream.fromEffect(
              MonixTask
                .deferAction(
                  implicit sc => MonixTask.eval(value.toReactivePublisher.toStream(queueSize).map(ev.resolve))
                )
                .to[Task]
            )
          )
        )
    }
}
