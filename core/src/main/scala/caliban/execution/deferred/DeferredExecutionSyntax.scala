package caliban.execution.deferred

import zio.URIO
import caliban.GraphQLResponse
import zio.stream.ZStream
import zio.{ZIO, Chunk}
import caliban.ResponseValue
import caliban.Value
import scala.language.implicitConversions

trait DeferredExecutionSyntax {
  implicit def responseDeferredSupport[R, E](response: URIO[R, GraphQLResponse[E]]): DeferredExecutionSyntax.Ops[R, E] =
      new DeferredExecutionSyntax.Ops[R, E](response)
}

object DeferredExecutionSyntax {
    class Ops[R, E](val response: URIO[R, GraphQLResponse[E]]) extends AnyVal {
        def toDeferred: ZIO[R, Nothing, (GraphQLResponse[E], ZStream[R, Nothing, ResponseValue])] = 
            response.map { original =>
                val extensions = original.extensions.toList.flatMap(_.fields.collect {
                  case ("__defer", ResponseValue.StreamValue(value)) => 
                      value.catchAll(t => ZStream.die(t))
                })
                val total = extensions.size
                (
                original.withoutExtension("__defer"),
                ZStream.concatAll(Chunk.fromIterable(extensions))
                   .zipWithIndex
                   .collect { 
                     case (ResponseValue.ObjectValue(f), index) =>
                       ResponseValue.ObjectValue(f ++ List("hasNext" -> Value.BooleanValue(index < total - 1)))
                   }
                )
            }
    }
}
