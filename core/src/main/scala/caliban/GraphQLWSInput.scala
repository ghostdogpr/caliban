package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

case class GraphQLWSInput(`type`: String, id: Option[String], payload: Option[InputValue])

object GraphQLWSInput extends GraphQLWSInputJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLWSInput]  =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLWSInput]  =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSInput] =
    caliban.interop.zio.GraphQLWSInputZioJson.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSInput]       =
    caliban.interop.tapir.schema.wsInputSchema.asInstanceOf[F[GraphQLWSInput]]
}
