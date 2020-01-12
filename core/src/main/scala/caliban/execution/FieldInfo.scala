package caliban.execution

case class FieldInfo(fieldName: String, path: List[Either[String, Int]], parentType: String, returnType: String)
