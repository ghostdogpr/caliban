package caliban

import zio.stream.Stream

package object ws {
  type Pipe[A, B]  = Stream[Throwable, A] => Stream[Throwable, B]
  type CalibanPipe = Pipe[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]
}
