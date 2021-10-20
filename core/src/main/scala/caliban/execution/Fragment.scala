package caliban.execution

import caliban.parsing.adt.Directive

final case class Fragment(
    name: Option[String] = None,
    directives: List[Directive] = Nil
)