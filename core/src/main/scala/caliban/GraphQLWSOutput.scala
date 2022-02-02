package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

case class GraphQLWSOutput(`type`: String, id: Option[String], payload: Option[ResponseValue])

object GraphQLWSOutput extends GraphQLWSOutputJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLWSOutput]  =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLWSOutput]  =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLWSOutput] =
    caliban.interop.zio.GraphQLWSOutputZioJson.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSOutput]       =
    caliban.interop.tapir.schema.wsOutputSchema.asInstanceOf[F[GraphQLWSOutput]]
}
