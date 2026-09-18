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
      value ::: fields.filterNot(_._1 == key)
    })))
}

object GraphQLResponse {
  private[caliban] def fromResponseValue(value: ResponseValue): Option[GraphQLResponse[CalibanError]] =
    value match {
      case response: ObjectValue =>
        for {
          errors     <- response.getOrNull("errors") match {
                          case null | NullValue  => AbsentField
                          case ListValue(values) => Some(Some(decodeErrors(values)))
                          case _                 => None
                        }
          extensions <- response.getOrNull("extensions") match {
                          case null | NullValue        => AbsentField
                          case extensions: ObjectValue => Some(Some(extensions))
                          case _                       => None
                        }
          hasNext    <- response.getOrNull("hasNext") match {
                          case null | NullValue      => AbsentField
                          case BooleanValue(hasNext) => Some(Some(hasNext))
                          case _                     => None
                        }
          decoded    <- fromDecoded(Option(response.getOrNull("data")), errors, extensions, hasNext)
        } yield decoded
      case _                     => None
    }

  private[caliban] def fromDecoded(
    data: Option[ResponseValue],
    errors: Option[List[CalibanError]],
    extensions: Option[ObjectValue],
    hasNext: Option[Boolean]
  ): Option[GraphQLResponse[CalibanError]] =
    (data, errors) match {
      case (None, None) => None
      case _            => Some(GraphQLResponse(data.getOrElse(NullValue), errors.getOrElse(Nil), extensions, hasNext))
    }

  private[caliban] def decodeErrors(values: List[ResponseValue]): List[CalibanError] =
    values.map(value => CalibanError.fromResponseValue(value).getOrElse(malformedRemoteError))

  private val malformedRemoteError: CalibanError.ExecutionError =
    CalibanError.ExecutionError(CalibanError.RemoteErrorMessage)

  private val AbsentField: Some[None.type] = Some(None)

  implicit def tapirSchema[F[_]: IsTapirSchema, E]: F[GraphQLResponse[E]] =
    caliban.interop.tapir.schema.responseSchema.asInstanceOf[F[GraphQLResponse[E]]]

  implicit def jsoniterCodec[E]: JsonValueCodec[GraphQLResponse[E]] =
    caliban.interop.jsoniter.GraphQLResponseJsoniter.graphQLResponseCodec
      .asInstanceOf[JsonValueCodec[GraphQLResponse[E]]]
}
