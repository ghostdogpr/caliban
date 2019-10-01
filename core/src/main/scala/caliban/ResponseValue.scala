package caliban

import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import zio.IO
import zio.stream.ZStream

sealed trait ResolvedValue
sealed trait ResponseValue extends ResolvedValue

/**
 * Resolved values that require more processing
 */
object ResolvedValue {
  case class ResolvedObjectValue(
    name: String,
    fields: Map[String, Map[String, Value] => IO[ExecutionError, ResolvedValue]]
  ) extends ResolvedValue
  case class ResolvedListValue(values: List[IO[ExecutionError, ResolvedValue]])  extends ResolvedValue
  case class ResolvedStreamValue(stream: ZStream[Any, Throwable, ResolvedValue]) extends ResolvedValue
}

/**
 * Resolved values fully processed, can be returned to client
 */
object ResponseValue {
  case object NullValue extends ResponseValue {
    override def toString: String = "null"
  }
  case class IntValue(value: Int) extends ResponseValue {
    override def toString: String = value.toString
  }
  case class FloatValue(value: Float) extends ResponseValue {
    override def toString: String = value.toString
  }
  case class StringValue(value: String) extends ResponseValue {
    override def toString: String = s""""$value""""
  }
  case class BooleanValue(value: Boolean) extends ResponseValue {
    override def toString: String = if (value) "true" else "false"
  }
  case class EnumValue(value: String) extends ResponseValue {
    override def toString: String = s""""$value""""
  }
  case class ListValue(values: List[ResponseValue]) extends ResponseValue {
    override def toString: String = values.mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, ResponseValue)]) extends ResponseValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
  }
  case class StreamValue(stream: ZStream[Any, Throwable, ResponseValue]) extends ResponseValue {
    override def toString: String = ""
  }
}
