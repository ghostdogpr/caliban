package caliban

import caliban.interop.play._

private[caliban] trait GraphQLResponseJsonCompat {
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads, E]: F[GraphQLResponse[E]]   =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseReads.asInstanceOf[F[GraphQLResponse[E]]]
}
