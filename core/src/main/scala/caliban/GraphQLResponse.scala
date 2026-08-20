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
  private[caliban] sealed trait ResponseField[+A]

  private[caliban] object ResponseField {
    case object Missing                   extends ResponseField[Nothing]
    final case class Present[A](value: A) extends ResponseField[A]
  }

  private[caliban] def decodeErrors(values: List[ResponseValue]): Option[List[CalibanError]] = {
    val decoded = values.map(CalibanError.fromResponseValue)
    if (decoded.forall(_.nonEmpty)) Some(decoded.flatten) else None
  }

  private[caliban] def fromDecoded(
    data: ResponseField[ResponseValue],
    errors: ResponseField[List[CalibanError]],
    extensions: Option[ResponseValue.ObjectValue],
    hasNext: Option[Boolean]
  ): Option[GraphQLResponse[CalibanError]] =
    (data, errors) match {
      case (_, ResponseField.Present(Nil))                => None
      case (ResponseField.Missing, ResponseField.Missing) => None
      case _                                              =>
        Some(
          GraphQLResponse(
            data = data match {
              case ResponseField.Present(value) => value
              case ResponseField.Missing        => NullValue
            },
            errors = errors match {
              case ResponseField.Present(value) => value
              case ResponseField.Missing        => Nil
            },
            extensions = extensions,
            hasNext = hasNext
          )
        )
    }

  private[caliban] def fromResponseValue(value: ResponseValue): Option[GraphQLResponse[CalibanError]] =
    value match {
      case ObjectValue(fields) =>
        val data       = fields.collectFirst { case ("data", value) => ResponseField.Present(value) }
          .getOrElse(ResponseField.Missing)
        val errors     = fields.collectFirst { case ("errors", value) => value } match {
          case None                    => Some(ResponseField.Missing)
          case Some(ListValue(values)) =>
            decodeErrors(values).map(ResponseField.Present(_))
          case _                       => None
        }
        val extensions = fields.collectFirst { case ("extensions", value) => value } match {
          case None                     => Some(None)
          case Some(value: ObjectValue) => Some(Some(value))
          case _                        => None
        }
        val hasNext    = fields.collectFirst { case ("hasNext", value) => value } match {
          case None                      => Some(None)
          case Some(BooleanValue(value)) => Some(Some(value))
          case _                         => None
        }
        for {
          decodedErrors <- errors
          extension     <- extensions
          next          <- hasNext
          response      <- fromDecoded(
                             data,
                             decodedErrors,
                             extension,
                             next
                           )
        } yield response
      case _                   => None
    }

  implicit def tapirSchema[F[_]: IsTapirSchema, E]: F[GraphQLResponse[E]] =
    caliban.interop.tapir.schema.responseSchema.asInstanceOf[F[GraphQLResponse[E]]]

  implicit def jsoniterCodec[E]: JsonValueCodec[GraphQLResponse[E]] =
    caliban.interop.jsoniter.GraphQLResponseJsoniter.graphQLResponseCodec
      .asInstanceOf[JsonValueCodec[GraphQLResponse[E]]]
}
