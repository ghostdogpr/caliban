package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }

private[caliban] trait GraphQLWSOutputJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSOutput]   =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputReads.asInstanceOf[F[GraphQLWSOutput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSOutput] =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputWrites.asInstanceOf[F[GraphQLWSOutput]]
}
