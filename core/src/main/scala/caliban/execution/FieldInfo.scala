package caliban.execution

import caliban.introspection.adt.__Type
import caliban.parsing.adt.Directive

case class FieldInfo(fieldName: String,
                     path: List[Either[String, Int]],
                     parentType: Option[__Type],
                     returnType: __Type,
                     directives: List[Directive] = Nil)
