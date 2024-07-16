package caliban

import caliban.Value.StringValue
import caliban.interop.tapir.IsTapirSchema
import caliban.rendering.ValueRenderer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import zio.stream.Stream

import scala.util.control.NonFatal
import scala.util.hashing.MurmurHash3

sealed trait InputValue extends Serializable { self =>
  def toInputString: String = ValueRenderer.inputValueRenderer.renderCompact(self)
}
object InputValue {
  case class ListValue(values: List[InputValue])          extends InputValue {
    override def toString: String      = values.mkString("[", ",", "]")
    override def toInputString: String = ValueRenderer.inputListValueRenderer.render(this)
  }
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")

    override def toInputString: String = ValueRenderer.inputObjectValueRenderer.render(this)
  }
  case class VariableValue(name: String)                  extends InputValue {
    override def toString: String = s"$$$name"
  }

  implicit def jsoniterCodec: JsonValueCodec[InputValue] =
    caliban.interop.jsoniter.ValueJsoniter.inputValueCodec
}

sealed trait ResponseValue extends Serializable { self =>

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
object ResponseValue {

  def at(path: List[PathValue])(value: ResponseValue): ResponseValue = {
    def loop(path: List[PathValue], value: ResponseValue): ResponseValue = path match {
      case Nil                            => value
      case PathValue.Key(key) :: tail     =>
        value match {
          case ObjectValue(fields) =>
            fields.find(_._1 == key) match {
              case Some((_, v)) => loop(tail, v)
              case None         => Value.NullValue
            }
          case ListValue(values)   =>
            ListValue(values.map(loop(path, _)))
          case _                   => Value.NullValue
        }
      case PathValue.Index(index) :: tail =>
        value match {
          case ListValue(values) =>
            val idx = index
            if (idx < values.size) {
              loop(tail, values(idx))
            } else {
              Value.NullValue
            }
          case _                 => Value.NullValue
        }
      case _                              => Value.NullValue
    }

    loop(path, value)
  }

  case class ListValue(values: List[ResponseValue])                extends ResponseValue {
    override def toString: String = ValueRenderer.responseListValueRenderer.renderCompact(this)
  }
  case class ObjectValue(fields: List[(String, ResponseValue)])    extends ResponseValue {
    override def toString: String =
      ValueRenderer.responseObjectValueRenderer.renderCompact(this)

    @transient override lazy val hashCode: Int = MurmurHash3.unorderedHash(fields)
    override def equals(other: Any): Boolean   =
      other match {
        case o: ObjectValue => o.hashCode == hashCode
        case _              => false
      }
  }
  case class StreamValue(stream: Stream[Throwable, ResponseValue]) extends ResponseValue {
    override def toString: String = "<stream>"
  }

  implicit def tapirSchema[F[_]: IsTapirSchema]: F[ResponseValue] =
    caliban.interop.tapir.schema.responseValueSchema.asInstanceOf[F[ResponseValue]]

  implicit def jsoniterCodec: JsonValueCodec[ResponseValue] =
    caliban.interop.jsoniter.ValueJsoniter.responseValueCodec
}

sealed trait Value extends InputValue with ResponseValue
object Value {
  case object NullValue                   extends Value                {
    override def toString: String = "null"
  }
  sealed trait IntValue                   extends Value                {
    def toInt: Int
    def toLong: Long
    def toBigInt: BigInt
  }
  sealed trait FloatValue                 extends Value                {
    def toFloat: Float
    def toDouble: Double
    def toBigDecimal: BigDecimal
  }
  case class StringValue(value: String)   extends Value with PathValue {
    override def toString: String = s""""${value.replace("\"", "\\\"").replace("\n", "\\n")}""""
  }
  case class BooleanValue(value: Boolean) extends Value                {
    override def toString: String = if (value) "true" else "false"
  }
  case class EnumValue(value: String)     extends Value                {
    override def toString: String      = s""""${value.replace("\"", "\\\"")}""""
    override def toInputString: String = ValueRenderer.enumInputValueRenderer.render(this)
  }

  object IntValue {
    def apply(v: Int): IntValue    = IntNumber(v)
    def apply(v: Long): IntValue   = LongNumber(v)
    def apply(v: BigInt): IntValue = BigIntNumber(v)

    @deprecated("Use `fromStringUnsafe` instead", "2.5.0")
    def apply(s: String): IntValue = fromStringUnsafe(s)

    @throws[NumberFormatException]("if the string is not a valid representation of an integer")
    def fromStringUnsafe(s: String): IntValue =
      try {
        val mod  = if (s.charAt(0) == '-') 1 else 0
        val size = s.length - mod
        if (size < 10) IntNumber(s.toInt)
        else if (size < 19) LongNumber(s.toLong)
        else BigIntNumber(BigInt(s))
      } catch {
        case NonFatal(_) => BigIntNumber(BigInt(s)) // Should never happen, but we leave it as a fallback
      }

    final case class IntNumber(value: Int)       extends IntValue with PathValue {
      override def toInt: Int       = value
      override def toLong: Long     = value.toLong
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    final case class LongNumber(value: Long)     extends IntValue                {
      override def toInt: Int       = value.toInt
      override def toLong: Long     = value
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    final case class BigIntNumber(value: BigInt) extends IntValue                {
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

    @deprecated("Use `fromStringUnsafe` instead", "2.6.0")
    def apply(s: String): FloatValue = fromStringUnsafe(s)

    @throws[NumberFormatException]("if the string is not a valid representation of a float")
    def fromStringUnsafe(s: String): FloatValue =
      try DoubleNumber(s.toDouble)
      catch { case NonFatal(_) => BigDecimalNumber(BigDecimal(s)) }

    final case class FloatNumber(value: Float)           extends FloatValue {
      override def toFloat: Float           = value
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = BigDecimal.decimal(value)
      override def toString: String         = value.toString
    }
    final case class DoubleNumber(value: Double)         extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value
      override def toBigDecimal: BigDecimal = BigDecimal(value)
      override def toString: String         = value.toString
    }
    final case class BigDecimalNumber(value: BigDecimal) extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = value
      override def toString: String         = value.toString
    }
  }
}

sealed trait PathValue extends Value {
  def isKey: Boolean = this match {
    case StringValue(_) => true
    case _              => false
  }
}

object PathValue {
  def fromEither(either: Either[String, Int]): PathValue = either.fold(Key.apply, Index.apply)

  object Key   {
    def apply(value: String): PathValue           = Value.StringValue(value)
    def unapply(value: PathValue): Option[String] = value match {
      case Value.StringValue(s) => Some(s)
      case _                    => None
    }
  }
  object Index {
    def apply(value: Int): PathValue           = Value.IntValue.IntNumber(value)
    def unapply(value: PathValue): Option[Int] = value match {
      case Value.IntValue.IntNumber(i) => Some(i)
      case _                           => None
    }
  }

  /**
   * This function parses a string and returns a PathValue.
   * If the string contains only digits, it returns a `PathValue.Index`.
   * Otherwise, it returns a `PathValue.Key`.
   *
   * @param value the string to parse
   * @return a PathValue which is either an `Index` if the string is numeric, or a `Key` otherwise
   */
  def parse(value: String): PathValue = {
    var i    = 0
    val size = value.length
    while (i < size) {
      val c = value.charAt(i)
      if (c >= '0' && c <= '9') i += 1
      else return PathValue.Key(value)
    }
    try PathValue.Index(value.toInt)
    catch { case NonFatal(_) => PathValue.Key(value) } // Should never happen, just a fallback
  }
}
