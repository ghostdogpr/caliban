package caliban.client

import scala.annotation.tailrec
import caliban.client.Value.NullValue

case class Argument[+A](name: String, value: A)(implicit encoder: ArgEncoder[A]) {
  def toGraphQL(
    useVariables: Boolean,
    variables: Map[String, (Value, String)]
  ): (String, Map[String, (Value, String)]) =
    encoder.encode(value) match {
      case NullValue => ("", variables)
      case v =>
        if (useVariables) {
          val variableName = Argument.generateVariableName(name, v, variables)
          (s"$name: $$$variableName", variables.updated(variableName, (v, encoder.formatTypeName)))
        } else {
          (s"$name: ${v.toString}", variables)
        }
    }
}

object Argument {

  @tailrec
  def generateVariableName(name: String, value: Value, variables: Map[String, (Value, String)]): String =
    variables.get(name) match {
      case None                       => name
      case Some((v, _)) if v == value => name
      case Some(_)                    => generateVariableName(name + "_", value, variables)
    }

}
