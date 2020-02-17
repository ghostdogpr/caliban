package caliban.interop.monix

import caliban.schema.{ Schema, SubscriptionSchema }
import caliban.{ GraphQL, GraphQLInterpreter, GraphQLResponse, InputValue }
import cats.effect.ConcurrentEffect
import monix.eval.Task
import monix.reactive.Observable
import zio.Runtime

package object implicits {

  implicit class MonixGraphQLInterpreter[R, E](underlying: GraphQLInterpreter[R, E]) {

    def executeAsync(
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, InputValue] = Map(),
      skipValidation: Boolean = false
    )(implicit runtime: Runtime[R]): Task[GraphQLResponse[E]] =
      MonixInterop.executeAsync(underlying)(
        query,
        operationName,
        variables,
        skipValidation
      )
  }

  implicit class MonixGraphQL[R, E](underlying: GraphQL[R]) {

    def checkAsync(query: String)(implicit runtime: Runtime[R]): Task[Unit] =
      MonixInterop.checkAsync(underlying)(query)
  }

  implicit def effectSchema[R, A](implicit ev: Schema[R, A], ev2: ConcurrentEffect[Task]): Schema[R, Task[A]] =
    MonixInterop.taskSchema

  implicit def observableSchema[R, A](
    implicit ev: Schema[R, A],
    ev2: ConcurrentEffect[Task]
  ): Schema[R, Observable[A]] =
    MonixInterop.observableSchema(16) // Size of the internal buffer. Use a power of 2 for best performance.

  implicit def observableSubscriptionSchema[A]: SubscriptionSchema[Observable[A]] =
    new SubscriptionSchema[Observable[A]] {}
}
