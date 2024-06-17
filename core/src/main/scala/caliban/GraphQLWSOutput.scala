package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonCodec, IsZIOJsonDecoder, IsZIOJsonEncoder }

case class GraphQLWSOutput(`type`: String, id: Option[String], payload: Option[ResponseValue])

object GraphQLWSOutput {
  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[GraphQLWSOutput]     =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLWSOutput]     =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def zioJsonCodec[F[_]: IsZIOJsonCodec]: F[GraphQLWSOutput]     =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputCodec.asInstanceOf[F[GraphQLWSOutput]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSOutput]       =
    caliban.interop.tapir.schema.wsOutputSchema.asInstanceOf[F[GraphQLWSOutput]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[GraphQLWSOutput]   =
    caliban.interop.jsoniter.GraphQLWSOutputJsoniter.graphQLWSOuputCodec.asInstanceOf[F[GraphQLWSOutput]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLWSOutput]   =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputReads.asInstanceOf[F[GraphQLWSOutput]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[GraphQLWSOutput] =
    caliban.interop.play.json.GraphQLWSOutputPlayJson.graphQLWSOutputWrites.asInstanceOf[F[GraphQLWSOutput]]

  @deprecated("kept for compatibility purposes only", "1.7.2")
  def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
  @deprecated("kept for compatibility purposes only", "1.7.2")
  def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
}
