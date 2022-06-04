package caliban.execution

import caliban.parsing.adt.Directive

case class Fragment(name: Option[String], directives: List[Directive])
