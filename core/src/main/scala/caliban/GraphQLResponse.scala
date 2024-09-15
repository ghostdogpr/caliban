package caliban

import caliban.ResponseValue._
import caliban.Value._
import caliban.interop.tapir.IsTapirSchema
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

import scala.collection.mutable.ListBuffer

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
    val builder     = new ListBuffer[(String, ResponseValue)]
    val hasErrors   = errors.nonEmpty
    val extensions0 = excludeExtensions match {
      case None       => extensions
      case Some(excl) =>
        extensions.flatMap { obj =>
          val newFields = obj.fields.filterNot(f => excl.contains(f._1))
          if (newFields.nonEmpty) Some(ObjectValue(newFields)) else None
        }
    }

    if (!hasErrors || keepDataOnErrors)
      builder += "data"       -> data
    if (hasErrors)
      builder += "errors"     -> ListValue(errors.map {
        case e: CalibanError => e.toResponseValue
        case e               => ObjectValue(List("message" -> StringValue(e.toString)))
      })
    if (extensions0.nonEmpty)
      builder += "extensions" -> extensions0.get
    if (hasNext.nonEmpty)
      builder += "hasNext"    -> BooleanValue(hasNext.get)

    ObjectValue(builder.result())
  }

  def withExtension(key: String, value: ResponseValue): GraphQLResponse[E] =
    copy(extensions = Some(ObjectValue(extensions.foldLeft(List(key -> value)) { case (value, ObjectValue(fields)) =>
      value ::: fields
    })))
}

object GraphQLResponse {
  implicit def tapirSchema[F[_]: IsTapirSchema, E]: F[GraphQLResponse[E]] =
    caliban.interop.tapir.schema.responseSchema.asInstanceOf[F[GraphQLResponse[E]]]

  private[caliban] implicit def jsoniterCodec[E]: JsonValueCodec[GraphQLResponse[E]] =
    caliban.interop.jsoniter.GraphQLResponseJsoniter.graphQLResponseCodec
      .asInstanceOf[JsonValueCodec[GraphQLResponse[E]]]
}
