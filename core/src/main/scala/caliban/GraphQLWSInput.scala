package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.tapir.IsTapirSchema

case class GraphQLWSInput(`type`: String, id: Option[String], payload: Option[InputValue])

object GraphQLWSInput extends GraphQLWSInputJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLWSInput] =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputEncoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLWSInput] =
    caliban.interop.circe.json.GraphQLWSInputCirce.graphQLWSInputDecoder.asInstanceOf[F[GraphQLWSInput]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSInput]      =
    caliban.interop.tapir.schema.wsInputSchema.asInstanceOf[F[GraphQLWSInput]]
}
