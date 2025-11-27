package caliban.client

import scala.annotation.tailrec
import caliban.client.__Value.__NullValue

/**
 * Represents an argument in a GraphQL query. Requires an encoder for the argument type.
 */
case class Argument[+A](name: String, value: A, typeInfo: String)(implicit encoder: ArgEncoder[A]) {
  def encodeRaw: __Value =
    encoder.encode(value)
}

object Argument {

  @tailrec
  def generateVariableName(
    name: String,
    value: __Value,
    variables: Map[String, (__Value, String)],
    index: Int = 0
  ): String = {
    val formattedName = if (index > 0) s"$name$index" else name
    variables.get(formattedName) match {
      case None                       => formattedName
      case Some((v, _)) if v == value => formattedName
      case Some(_)                    =>
        generateVariableName(name, value, variables, index + 1)
    }
  }

}
