package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

private[caliban] trait GraphQLWSInputJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSInput]   =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputReads.asInstanceOf[F[GraphQLWSInput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSInput] =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputWrites.asInstanceOf[F[GraphQLWSInput]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
}
