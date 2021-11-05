package caliban

import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }

case class GraphQLWSOutput(`type`: String, id: Option[String], payload: Option[ResponseValue])

object GraphQLWSOutput extends GraphQLWSOutputJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLWSOutput] =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputEncoder.asInstanceOf[F[GraphQLWSOutput]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLWSOutput] =
    caliban.interop.circe.json.GraphQLWSOutputCirce.graphQLWSOutputDecoder.asInstanceOf[F[GraphQLWSOutput]]
}
