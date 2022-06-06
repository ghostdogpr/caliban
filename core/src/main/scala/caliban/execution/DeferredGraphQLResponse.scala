package caliban.execution

import caliban.ResponseValue.StreamValue
import caliban.{ GraphQLResponse, ResponseValue }
import zio.stream.ZStream

case class DeferredGraphQLResponse[+E] private (
  head: GraphQLResponse[E],
  tail: ZStream[Any, Throwable, ResponseValue]
)

object DeferredGraphQLResponse {
  def apply[E](response: GraphQLResponse[E]): DeferredGraphQLResponse[E] =
    DeferredGraphQLResponse(
      response,
      ZStream
        .fromIterable(response.extensions.flatMap(_.fields.collectFirst { case ("__defer", StreamValue(stream)) =>
          stream
        }))
        .flatten
    )
}
