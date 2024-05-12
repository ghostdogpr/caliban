package caliban.interop.monix

import caliban._
import caliban.introspection.adt.__Type
import caliban.schema.Step.{ QueryStep, StreamStep }
import caliban.schema.{ Schema, Step }
import cats.effect.ConcurrentEffect
import monix.eval.{ Task => MonixTask }
import monix.reactive.Observable
import zio._
import zio.interop.catz._
import zio.interop.reactivestreams._
import zio.query.ZQuery
import zio.stream.ZStream

object MonixInterop {

  def executeAsync[R, E](graphQL: GraphQLInterpreter[R, E])(
    query: String,
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    extensions: Map[String, InputValue] = Map()
  )(implicit runtime: Runtime[R]): MonixTask[GraphQLResponse[E]] =
    graphQL
      .execute(query, operationName, variables, extensions)
      .toMonixTask

  def checkAsync[R](
    graphQL: GraphQLInterpreter[R, Any]
  )(query: String)(implicit runtime: Runtime[Any]): MonixTask[Unit] =
    graphQL.check(query).toMonixTask

  @deprecated("Use interpreterMonix instead")
  def interpreterAsync[R](
    graphQL: GraphQL[R]
  )(implicit runtime: Runtime[Any]): MonixTask[GraphQLInterpreter[R, CalibanError]] =
    interpreterMonix(graphQL)

  def interpreterMonix[R](
    graphQL: GraphQL[R]
  ): MonixTask[GraphQLInterpreter[R, CalibanError]] =
    MonixTask.fromEither(graphQL.interpreterEither)

  implicit final class ExtraZioEffectOps[-R, +A](private val effect: ZIO[R, Throwable, A]) extends AnyVal {
    def toMonixTask(implicit zioRuntime: Runtime[R]): MonixTask[A] =
      MonixTask.cancelable { cb =>
        val workflow = effect.onExit(
          _.foldCauseZIO(
            cause => ZIO.succeed(cb.onError(cause.squashTrace)),
            value => ZIO.succeed(cb.onSuccess(value))
          )
        )
        Unsafe.unsafe { implicit unsafe =>
          val fiber = zioRuntime.unsafe.fork(workflow)
          MonixTask.eval(zioRuntime.unsafe.run(fiber.interrupt)).void
        }
      }
  }

  def taskSchema[R, A](implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, MonixTask[A]] =
    new Schema[R, MonixTask[A]] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def resolve(value: MonixTask[A]): Step[R]                     =
        QueryStep(ZQuery.fromZIO(value.to[Task].map(ev.resolve)))
    }

  def observableSchema[R, A](
    queueSize: Int
  )(implicit ev: Schema[R, A], ev2: ConcurrentEffect[MonixTask]): Schema[R, Observable[A]] =
    new Schema[R, Observable[A]] {
      override def nullable: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else (if (ev.nullable) t else t.nonNull).list
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
