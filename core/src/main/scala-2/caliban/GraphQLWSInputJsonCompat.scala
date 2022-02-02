package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }

private[caliban] trait GraphQLWSInputJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSInput]   =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputReads.asInstanceOf[F[GraphQLWSInput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSInput] =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputWrites.asInstanceOf[F[GraphQLWSInput]]
}
