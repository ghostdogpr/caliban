package caliban.parsing.adt

import caliban.InputValue

final case class Directive(name: String, arguments: Map[String, InputValue] = Map.empty, index: Int = 0)
