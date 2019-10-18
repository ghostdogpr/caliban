package caliban

import zio.stream.Stream

sealed trait ResponseValue {
  def asInt: Option[Int]                                 = None
  def asLong: Option[Long]                               = None
  def asFloat: Option[Float]                             = None
  def asDouble: Option[Double]                           = None
  def asString: Option[String]                           = None
  def asBoolean: Option[Boolean]                         = None
  def asList: Option[List[ResponseValue]]                = None
  def asMap: Option[Map[String, ResponseValue]]          = None
  def asStream: Option[Stream[Throwable, ResponseValue]] = None
}

object ResponseValue {
  case object NullValue extends ResponseValue {
    override def toString: String = "null"
  }
  case class IntValue(value: Long) extends ResponseValue {
    override def toString: String     = value.toString
    override def asInt: Option[Int]   = Some(value.toInt)
    override def asLong: Option[Long] = Some(value)
  }
  case class FloatValue(value: Double) extends ResponseValue {
    override def toString: String         = value.toString
    override def asFloat: Option[Float]   = Some(value.toFloat)
    override def asDouble: Option[Double] = Some(value)
  }
  case class StringValue(value: String) extends ResponseValue {
    override def toString: String         = s""""${value.replace("\"", "\\\"")}""""
    override def asString: Option[String] = Some(value)
  }
  case class BooleanValue(value: Boolean) extends ResponseValue {
    override def toString: String           = if (value) "true" else "false"
    override def asBoolean: Option[Boolean] = Some(value)
  }
  case class EnumValue(value: String) extends ResponseValue {
    override def toString: String         = s""""${value.replace("\"", "\\\"")}""""
    override def asString: Option[String] = Some(value)
  }
  case class ListValue(values: List[ResponseValue]) extends ResponseValue {
    override def toString: String                    = values.mkString("[", ",", "]")
    override def asList: Option[List[ResponseValue]] = Some(values)
  }
  case class ObjectValue(fields: List[(String, ResponseValue)]) extends ResponseValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
    override def asMap: Option[Map[String, ResponseValue]] = Some(fields.toMap)
  }
  case class StreamValue(stream: Stream[Throwable, ResponseValue]) extends ResponseValue {
    override def toString: String                                   = "<stream>"
    override def asStream: Option[Stream[Throwable, ResponseValue]] = Some(stream)
  }
}
