package caliban.parsing.adt

import caliban.InputValue

case class Directive(name: String, arguments: Map[String, InputValue], index: Int)
