package caliban.execution

import caliban.introspection.adt.__Type

case class FieldInfo(fieldName: String, path: List[Either[String, Int]], parentType: Option[__Type], returnType: __Type)
