package caliban.client

import com.github.plokhotnyuk.jsoniter_scala.core.{ writeToString, JsonReader, JsonValueCodec, JsonWriter }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.annotation.switch
import scala.collection.immutable.TreeMap

/**
 * Value that can be returned by the server or sent as an argument.
 */
sealed trait __Value { self =>
  def dropNullValues: __Value = self match {
    case __Value.__ListValue(values)   => __Value.__ListValue(values.map(_.dropNullValues))
    case __Value.__ObjectValue(fields) =>
      __Value.__ObjectValue(fields.flatMap { case (name, value) =>
        value match {
          case __Value.__NullValue => None
          case _                   => Some(name -> value.dropNullValues)
        }
      })
    case _                             => self
  }
}

object __Value {
  private final val MaxDepth                                    = 1023
  private implicit val stringCodecMaker: JsonValueCodec[String] = JsonCodecMaker.make[String]

  case object __NullValue                                   extends __Value {
    override def toString: String = "null"
  }
  case class __NumberValue(value: BigDecimal)               extends __Value {
    override def toString: String = s"$value"
  }
  case class __EnumValue(value: String)                     extends __Value {
    override def toString: String = value
  }
  case class __StringValue(value: String)                   extends __Value {
    override def toString: String = writeToString(value)
  }
  case class __BooleanValue(value: Boolean)                 extends __Value {
    override def toString: String = value.toString
  }
  case class __ListValue(values: List[__Value])             extends __Value {
    override def toString: String = values.map(_.toString).mkString("[", ",", "]")
  }
  case class __ObjectValue(fields: List[(String, __Value)]) extends __Value {
    override def toString: String =
      fields.map { case (name, value) => s"""$name:${value.toString}""" }.mkString("{", ",", "}")
  }

  object __ObjectValue {
    implicit val codec: JsonValueCodec[__ObjectValue] = new JsonValueCodec[__ObjectValue] {
      def decodeValue(in: JsonReader, default: __ObjectValue): __ObjectValue = {
        val b = in.nextToken()
        if (b == '{') {
          in.rollbackToken()
          decodeJsonValue(in, MaxDepth) match {
            case v: __ObjectValue => v
            case v                => in.decodeError(s"expected object but received $v")
          }
        } else in.decodeError(s"expected object but received $b")
      }

      def encodeValue(x: __ObjectValue, out: JsonWriter): Unit =
        encodeJsonValue(x, out, MaxDepth)

      def nullValue: __ObjectValue =
        null.asInstanceOf[__ObjectValue]
    }
  }

  implicit val jsonCodec: JsonValueCodec[__Value] = new JsonValueCodec[__Value] {
    def decodeValue(in: JsonReader, default: __Value): __Value =
      decodeJsonValue(in, MaxDepth)

    def encodeValue(x: __Value, out: JsonWriter): Unit =
      encodeJsonValue(x, out, MaxDepth)

    def nullValue: __Value = __NullValue
  }

  private val emptyValueObject = __ObjectValue(Nil)
  private val emptyValueList   = __ListValue(Nil)

  private def encodeJsonValue(x: __Value, out: JsonWriter, depth: Int): Unit = x match {
    case __NumberValue(value)  => out.writeVal(value)
    case __EnumValue(value)    => out.writeVal(value)
    case __StringValue(value)  => out.writeVal(value)
    case __BooleanValue(value) => out.writeVal(value)
    case __ListValue(values)   =>
      val newDepth = depth - 1
      if (newDepth < 0) out.encodeError("Max depth reached")
      out.writeArrayStart()
      values.foreach(encodeJsonValue(_, out, depth - 1))
      out.writeArrayEnd()
    case __ObjectValue(fields) =>
      val newDepth = depth - 1
      if (newDepth < 0) out.encodeError("Max depth reached")
      out.writeObjectStart()
      fields.foreach { case (name, value) =>
        out.writeKey(name)
        encodeJsonValue(value, out, depth - 1)
      }
      out.writeObjectEnd()
    case `__NullValue`         => out.writeNull()
  }

  private def decodeJsonValue(in: JsonReader, depth: Int): __Value = {
    val b = in.nextToken()
    (b: @switch) match {
      case 'n'                                                             =>
        in.readNullOrError(__NullValue, "unexpected JSON value")
      case 't' | 'f'                                                       =>
        in.rollbackToken()
        __BooleanValue(in.readBoolean())
      case '{'                                                             =>
        val newDepth = depth - 1
        if (newDepth < 0) in.decodeError("Max depth reached")
        else if (in.isNextToken('}')) emptyValueObject
        else {
          in.rollbackToken()
          val fields = TreeMap.newBuilder[String, __Value]
          while ({
            fields += in.readKeyAsString() -> decodeJsonValue(in, newDepth)
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) __ObjectValue(fields.result().toList)
          else in.objectEndOrCommaError()
        }
      case '['                                                             =>
        val newDepth = depth - 1
        if (newDepth < 0) in.decodeError("Max depth reached")
        else if (in.isNextToken(']')) emptyValueList
        else {
          in.rollbackToken()
          val values = Array.newBuilder[__Value]
          while ({
            values += decodeJsonValue(in, newDepth)
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken(']')) __ListValue(values.result().toList)
          else in.arrayEndOrCommaError()
        }
      case '"'                                                             =>
        in.rollbackToken()
        __StringValue(in.readString(null))
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' =>
        in.rollbackToken()
        __NumberValue(in.readBigDecimal(null))
      case c                                                               =>
        in.decodeError(s"unexpected token: $c")
    }
  }

}
