package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonCodec, IsZIOJsonDecoder, IsZIOJsonEncoder }

case class GraphQLWSInput(`type`: String, id: Option[String], payload: Option[InputValue])

object GraphQLWSInput {
  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[GraphQLWSInput]     =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLWSInput]     =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def zioJsonCodec[F[_]: IsZIOJsonCodec]: F[GraphQLWSInput]     =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputCodec.asInstanceOf[F[GraphQLWSInput]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSInput]       =
    caliban.interop.tapir.schema.wsInputSchema.asInstanceOf[F[GraphQLWSInput]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[GraphQLWSInput]   =
    caliban.interop.jsoniter.GraphQLWSInputJsoniter.graphQLWSInputCodec.asInstanceOf[F[GraphQLWSInput]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSInput]   =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputReads.asInstanceOf[F[GraphQLWSInput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSInput] =
    caliban.interop.play.json.GraphQLWSInputPlayJson.graphQLWSInputWrites.asInstanceOf[F[GraphQLWSInput]]

  @deprecated("kept for compatibility purposes only", "1.7.2")
  def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  @deprecated("kept for compatibility purposes only", "1.7.2")
  def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
}
