package caliban

import caliban.interop.play.IsPlayJsonReads
import caliban.interop.zio.IsZIOJsonDecoder

private[caliban] trait GraphQLRequestJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest]   =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLRequest] =
    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
}
