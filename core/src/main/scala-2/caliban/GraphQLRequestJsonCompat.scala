package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }

private[caliban] trait GraphQLRequestJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest]   =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLRequest] =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestWrites.asInstanceOf[F[GraphQLRequest]]
}
