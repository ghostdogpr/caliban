package caliban

import caliban.ResponseValue._
import caliban.Value._
import caliban.interop.circe._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None) {
  def toResponseValue: ResponseValue =
    ObjectValue(
      List(
        "data"       -> Some(data),
        "errors"     -> (if (errors.nonEmpty)
                       Some(ListValue(errors.map {
                         case e: CalibanError => e.toResponseValue
                         case e               => StringValue(e.toString)
                       }))
                     else None),
        "extensions" -> extensions
      ).collect { case (name, Some(v)) => name -> v }
    )
}

object GraphQLResponse extends GraphQLResponseJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]
}
