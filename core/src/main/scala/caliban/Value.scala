package caliban

import scala.util.Try
import caliban.interop.circe._
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }
import zio.stream.Stream

sealed trait InputValue {
  def toInputString: String = toString
}
object InputValue extends ValueJsonCompat {
  case class ListValue(values: List[InputValue])          extends InputValue {
    override def toString: String      = values.mkString("[", ",", "]")
    override def toInputString: String = values.map(_.toInputString).mkString("[", ", ", "]")
  }
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name:${value.toString}"""" }.mkString("{", ",", "}")

    override def toInputString: String =
      fields.map { case (name, value) => s"""$name: ${value.toInputString}""" }.mkString("{", ", ", "}")
  }
  case class VariableValue(name: String)                  extends InputValue {
    override def toString: String = s"$$$name"
  }

  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[InputValue]               =
    caliban.interop.circe.json.ValueCirce.inputValueEncoder.asInstanceOf[F[InputValue]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[InputValue]               =
    caliban.interop.circe.json.ValueCirce.inputValueDecoder.asInstanceOf[F[InputValue]]
  implicit def inputValueZioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[InputValue] =
    caliban.interop.zio.ValueZIOJson.inputValueEncoder.asInstanceOf[F[InputValue]]
  implicit def inputValueZioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[InputValue] =
    caliban.interop.zio.ValueZIOJson.inputValueDecoder.asInstanceOf[F[InputValue]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[InputValue]             =
    caliban.interop.jsoniter.json.inputValueCodec.asInstanceOf[F[InputValue]]
}

sealed trait ResponseValue { self =>

  /**
   * Performs a deep merge of two response values. This currently means that the list values will be concatenated, and
   * object values will be merged, incompatible types will assume the right-hand side of the merge.
   */
  def deepMerge(other: ResponseValue): ResponseValue = (this, other) match {
    case (ResponseValue.ObjectValue(o1), ResponseValue.ObjectValue(o2)) =>
      val otherMap = o2.toMap
      ResponseValue.ObjectValue(o1.map { case (k, v) =>
        otherMap.get(k) match {
          case Some(otherValue) => (k, v.deepMerge(otherValue))
          case None             => (k, v)
        }
      })
    case (ResponseValue.ListValue(l1), ResponseValue.ListValue(l2))     =>
      ResponseValue.ListValue(l1 ++ l2)
    case _                                                              => other
  }
}
object ResponseValue extends ValueJsonCompat {
  case class ListValue(values: List[ResponseValue])                extends ResponseValue {
    override def toString: String = values.mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, ResponseValue)])    extends ResponseValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")

    override def hashCode: Int               = fields.toSet.hashCode()
    override def equals(other: Any): Boolean =
      other match {
        case o: ObjectValue => o.hashCode() == hashCode
        case _              => false
      }
  }
  case class StreamValue(stream: Stream[Throwable, ResponseValue]) extends ResponseValue {
    override def toString: String = "<stream>"
  }

  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[ResponseValue]                  =
    caliban.interop.circe.json.ValueCirce.responseValueEncoder.asInstanceOf[F[ResponseValue]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[ResponseValue]                  =
    caliban.interop.circe.json.ValueCirce.responseValueDecoder.asInstanceOf[F[ResponseValue]]
  implicit def responseValueZioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[ResponseValue] =
    caliban.interop.zio.ValueZIOJson.responseValueEncoder.asInstanceOf[F[ResponseValue]]
  implicit def responseValueZioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[ResponseValue] =
    caliban.interop.zio.ValueZIOJson.responseValueDecoder.asInstanceOf[F[ResponseValue]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[ResponseValue]                =
    caliban.interop.jsoniter.json.responseValueCodec.asInstanceOf[F[ResponseValue]]
}

sealed trait Value extends InputValue with ResponseValue
object Value {
  case object NullValue                   extends Value {
    override def toString: String = "null"
  }
  sealed trait IntValue                   extends Value {
    def toInt: Int
    def toLong: Long
    def toBigInt: BigInt
  }
  sealed trait FloatValue                 extends Value {
    def toFloat: Float
    def toDouble: Double
    def toBigDecimal: BigDecimal
  }
  case class StringValue(value: String)   extends Value {
    override def toString: String = s""""${value.replace("\"", "\\\"").replace("\n", "\\n")}""""
  }
  case class BooleanValue(value: Boolean) extends Value {
    override def toString: String = if (value) "true" else "false"
  }
  case class EnumValue(value: String)     extends Value {
    override def toString: String      = s""""${value.replace("\"", "\\\"")}""""
    override def toInputString: String = s"""${value.replace("\"", "\\\"")}"""
  }

  object IntValue {
    def apply(v: Int): IntValue    = IntNumber(v)
    def apply(v: Long): IntValue   = LongNumber(v)
    def apply(v: BigInt): IntValue = BigIntNumber(v)
    def apply(s: String): IntValue =
      Try(IntNumber(s.toInt)) orElse
        Try(LongNumber(s.toLong)) getOrElse
        BigIntNumber(BigInt(s))

    case class IntNumber(value: Int)       extends IntValue {
      override def toInt: Int       = value
      override def toLong: Long     = value.toLong
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    case class LongNumber(value: Long)     extends IntValue {
      override def toInt: Int       = value.toInt
      override def toLong: Long     = value
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    case class BigIntNumber(value: BigInt) extends IntValue {
      override def toInt: Int       = value.toInt
      override def toLong: Long     = value.toLong
      override def toBigInt: BigInt = value
      override def toString: String = value.toString
    }
  }

  object FloatValue {
    def apply(v: Float): FloatValue      = FloatNumber(v)
    def apply(v: Double): FloatValue     = DoubleNumber(v)
    def apply(v: BigDecimal): FloatValue = BigDecimalNumber(v)
    def apply(s: String): FloatValue     = BigDecimalNumber(BigDecimal(s))

    case class FloatNumber(value: Float)           extends FloatValue {
      override def toFloat: Float           = value
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = BigDecimal.decimal(value)
      override def toString: String         = value.toString
    }
    case class DoubleNumber(value: Double)         extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value
      override def toBigDecimal: BigDecimal = BigDecimal(value)
      override def toString: String         = value.toString
    }
    case class BigDecimalNumber(value: BigDecimal) extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = value
      override def toString: String         = value.toString
    }
  }
}
