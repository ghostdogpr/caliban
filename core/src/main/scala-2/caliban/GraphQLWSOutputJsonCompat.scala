package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

private[caliban] trait GraphQLWSOutputJsonCompat {
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSOutput]   =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputReads.asInstanceOf[F[GraphQLWSOutput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSOutput] =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputWrites.asInstanceOf[F[GraphQLWSOutput]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
}
