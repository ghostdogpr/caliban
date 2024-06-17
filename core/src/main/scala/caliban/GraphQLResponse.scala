package caliban

import caliban.ResponseValue._
import caliban.Value._
import caliban.interop.circe._
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.IsZIOJsonCodec

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](
  data: ResponseValue,
  errors: List[E],
  extensions: Option[ObjectValue] = None,
  hasNext: Option[Boolean] = None
) {
  def toResponseValue: ResponseValue = toResponseValue(keepDataOnErrors = true)

  def toResponseValue(keepDataOnErrors: Boolean, excludeExtensions: Option[Set[String]] = None): ResponseValue = {
    val hasErrors = errors.nonEmpty
    ObjectValue(
      List(
        "data"       -> (if (!hasErrors || keepDataOnErrors) Some(data) else None),
        "errors"     -> (if (hasErrors)
                       Some(ListValue(errors.map {
                         case e: CalibanError => e.toResponseValue
                         case e               => ObjectValue(List("message" -> StringValue(e.toString)))
                       }))
                     else None),
        "extensions" -> excludeExtensions.fold(extensions)(excl =>
          extensions.map(obj => ObjectValue(obj.fields.filterNot(f => excl.contains(f._1))))
        ),
        "hasNext"    -> hasNext.map(BooleanValue.apply)
      ).collect { case (name, Some(v)) => name -> v }
    )
  }

  def withExtension(key: String, value: ResponseValue): GraphQLResponse[E] =
    copy(extensions = Some(ObjectValue(extensions.foldLeft(List(key -> value)) { case (value, ObjectValue(fields)) =>
      value ::: fields
    })))
}

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]]     =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponse[E]]     =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonCodec[F[_]: IsZIOJsonCodec, E]: F[GraphQLResponse[E]]     =
    caliban.interop.zio.GraphQLResponseZioJson.graphQLResponseCodec.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def tapirSchema[F[_]: IsTapirSchema, E]: F[GraphQLResponse[E]]       =
    caliban.interop.tapir.schema.responseSchema.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec, E]: F[GraphQLResponse[E]]   =
    caliban.interop.jsoniter.GraphQLResponseJsoniter.graphQLResponseCodec.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads, E]: F[GraphQLResponse[E]]   =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseReads.asInstanceOf[F[GraphQLResponse[E]]]
}
