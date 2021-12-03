package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

private[caliban] trait GraphQLRequestJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest]   =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLRequest] =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestWrites.asInstanceOf[F[GraphQLRequest]]
//  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLRequest] =
//    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
//  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLRequest] =
//    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestEncoder.asInstanceOf[F[GraphQLRequest]]
}
