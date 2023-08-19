package caliban.rendering

import caliban.Value.{ FloatValue, IntValue, StringValue }
import caliban.{ InputValue, ResponseValue, Value }

import scala.annotation.switch

object ValueRenderer {

  lazy val inputValueRenderer: Renderer[InputValue] = new Renderer[InputValue] {
    override protected[caliban] def unsafeRender(value: InputValue, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case InputValue.ListValue(values)   =>
          write += '['
          var first = true
          values.foreach { value =>
            if (first) first = false
            else write ++= ", "
            unsafeRender(value, indent, write)
          }
          write += ']'
        case InputValue.ObjectValue(fields) =>
          write += '{'
          var first = true
          fields.foreach { field =>
            if (first) first = false
            else write ++= ", "
            write ++= field._1
            write += ':'
            write += ' '
            unsafeRender(field._2, indent, write)
          }
          write += '}'
        case InputValue.VariableValue(name) =>
          write += '$'
          write ++= name
        case StringValue(str)               =>
          write += '"'
          unsafeFastEscape(str, write)
          write += '"'
        case Value.EnumValue(value)         => unsafeFastEscape(value, write)
        case Value.BooleanValue(value)      => if (value) write ++= "true" else write ++= "false"
        case Value.NullValue                => write ++= "null"
        case v                              => write ++= v.toInputString
      }

  }

  lazy val responseValueRenderer: Renderer[ResponseValue] = new Renderer[ResponseValue] {
    override protected[caliban] def unsafeRender(
      value: ResponseValue,
      indent: Option[Int],
      write: StringBuilder
    ): Unit =
      value match {
        case ResponseValue.ListValue(values)   =>
          write += '['
          var first = true
          values.foreach { value =>
            if (first) first = false
            else write += ','
            unsafeRender(value, indent, write)
          }
          write += ']'
        case ResponseValue.ObjectValue(fields) =>
          write += '{'
          var first = true
          fields.foreach { field =>
            if (first) first = false
            else write ++= ", "
            write ++= field._1
            write += ':'
            write += ' '
            unsafeRender(field._2, indent, write)
          }
          write += '}'
        case StringValue(str)                  =>
          write += '"'
          unsafeFastEscape(str, write)
          write += '"'
        case Value.EnumValue(value)            =>
          write += '"'
          unsafeFastEscape(value, write)
          write += '"'
        case Value.BooleanValue(value)         => if (value) write ++= "true" else write ++= "false"
        case _: ResponseValue.StreamValue      => write ++= "<stream>"
        case Value.NullValue                   => write ++= "null"
        case v                                 => write ++= v.toString
      }
  }

  private def unsafeFastEscape(str: String, write: StringBuilder): Unit = {
    var i = 0
    while (i < str.length) {
      (str.charAt(i): @switch) match {
        case '"'  => write ++= "\\\""
        case '\n' => write ++= "\\n"
        case c    => write += c
      }
      i += 1
    }
  }

}
