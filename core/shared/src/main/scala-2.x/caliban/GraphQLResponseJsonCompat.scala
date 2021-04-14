package caliban

import caliban.interop.play._
import caliban.interop.zio.IsZIOJsonEncoder

private[caliban] trait GraphQLResponseJsonCompat {
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.zio.GraphQLResponseZioJson.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
}
