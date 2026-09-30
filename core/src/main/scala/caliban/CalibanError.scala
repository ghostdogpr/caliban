package caliban

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

import scala.util.control.NoStackTrace

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends NoStackTrace with Product with Serializable {
  def msg: String
  override def getMessage: String = msg

  def toResponseValue: ResponseValue
}

object CalibanError {

  private[caliban] final val RemoteErrorMessage = "Remote GraphQL request failed."

  private[caliban] def fromResponseValue(value: ResponseValue): Option[CalibanError] =
    value match {
      case error: ObjectValue =>
        val message    = error.getOrNull("message") match {
          case StringValue(message) => message
          case _                    => RemoteErrorMessage
        }
        val path       = error.getOrNull("path") match {
          case ListValue(values) => decodeAll(values)(decodePathValue).getOrElse(Nil)
          case _                 => Nil
        }
        val location   = error.getOrNull("locations") match {
          case ListValue(values) => decodeAll(values)(decodeLocation).flatMap(_.headOption)
          case _                 => None
        }
        val extensions = error.getOrNull("extensions") match {
          case extensions: ObjectValue => Some(extensions)
          case _                       => None
        }
        Some(ExecutionError(message, path, location, extensions = extensions))
      case _                  => None
    }

  private def decodeAll[A](values: List[ResponseValue])(decode: ResponseValue => Option[A]): Option[List[A]] = {
    val decoded = values.map(decode)
    if (decoded.forall(_.isDefined)) Some(decoded.flatten) else None
  }

  private def decodePathValue(value: ResponseValue): Option[PathValue] =
    value match {
      case key: StringValue          => Some(key)
      case index: IntValue.IntNumber => Some(index)
      case _                         => None
    }

  private def decodeLocation(value: ResponseValue): Option[LocationInfo] =
    value match {
      case ObjectValue(fields) =>
        for {
          line   <- fields.collectFirst { case ("line", IntValue.IntNumber(line)) => line }
          column <- fields.collectFirst { case ("column", IntValue.IntNumber(column)) => column }
        } yield LocationInfo(column, line)
      case _                   => None
    }

  /**
   * Describes an error that happened while parsing a query.
   */
  case class ParsingError(
    msg: String,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String      = s"Parsing Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable   = innerThrowable.orNull
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(s"Parsing Error: $msg")),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
  }

  /**
   * Describes an error that happened while validating a query.
   */
  case class ValidationError(
    msg: String,
    explanatoryText: String,
    locationInfo: Option[LocationInfo] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String      = s"ValidationError Error: $msg"
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(msg)),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
  }

  /**
   * Describes an error that happened while executing a query.
   */
  case class ExecutionError(
    msg: String,
    path: List[PathValue] = Nil,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String      = s"Execution Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable   = innerThrowable.orNull
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(msg)),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "path"       -> Some(path).collect { case p if p.nonEmpty => ListValue(p) },
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
  }

  private[caliban] implicit def jsoniterCodec: JsonValueCodec[CalibanError] =
    caliban.interop.jsoniter.ErrorJsoniter.errorValueCodec
}
