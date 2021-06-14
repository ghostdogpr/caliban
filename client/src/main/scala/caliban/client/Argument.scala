package caliban.client

import scala.annotation.tailrec
import caliban.client.__Value.__NullValue

/**
 * Represents an argument in a GraphQL query. Requires an encoder for the argument type.
 */
case class Argument[+A](name: String, value: A, typeInfo: String)(implicit encoder: ArgEncoder[A]) {
  def toGraphQL(
    useVariables: Boolean,
    dropNullInputValues: Boolean,
    variables: Map[String, (__Value, String)]
  ): (String, Map[String, (__Value, String)]) =
    encoder.encode(value) match {
      case `__NullValue` => ("", variables)
      case v             =>
        val value = if (dropNullInputValues) v.dropNullValues else v
        if (useVariables) {
          val variableName = Argument.generateVariableName(name, value, variables)
          (s"$name:$$$variableName", variables.updated(variableName, (value, typeInfo)))
        } else {
          (s"$name:${value.toString}", variables)
        }
    }
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
