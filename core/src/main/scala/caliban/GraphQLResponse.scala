package caliban

import caliban.ResponseValue._
import caliban.Value._
import caliban.interop.circe._
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](
  data: ResponseValue,
  errors: List[E],
  extensions: Option[ObjectValue] = None,
  hasNext: Option[Boolean] = None
) {
  def toResponseValue: ResponseValue =
    ObjectValue(
      List(
        "data"       -> Some(data),
        "errors"     -> (if (errors.nonEmpty)
                       Some(ListValue(errors.map {
                         case e: CalibanError => e.toResponseValue
                         case e               => ObjectValue(List("message" -> StringValue(e.toString)))
                       }))
                     else None),
        "extensions" -> extensions,
        "hasNext"    -> hasNext.map(BooleanValue.apply)
      ).collect { case (name, Some(v)) => name -> v }
    )

  def withExtension(key: String, value: ResponseValue): GraphQLResponse[E] =
    copy(extensions = Some(ObjectValue(extensions.foldLeft(List(key -> value)) { case (value, ObjectValue(fields)) =>
      value ::: fields
    })))
}

object GraphQLResponse extends GraphQLResponseJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]]     =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponse[E]]     =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.zio.GraphQLResponseZioJson.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.zio.GraphQLResponseZioJson.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def tapirSchema[F[_]: IsTapirSchema, E]: F[GraphQLResponse[E]]       =
    caliban.interop.tapir.schema.responseSchema.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec, E]: F[GraphQLResponse[E]]   =
    caliban.interop.jsoniter.GraphQLResponseJsoniter.graphQLResponseCodec.asInstanceOf[F[GraphQLResponse[E]]]
}
