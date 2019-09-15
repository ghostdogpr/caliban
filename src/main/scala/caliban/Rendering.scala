package caliban

import caliban.schema.Types.{ EnumValue, Field, InputValue, Type, TypeKind }

object Rendering {

  def renderTypes(types: Map[String, Type]): String =
    types.map {
      case (_, t) =>
        t.kind match {
          case TypeKind.SCALAR   => ""
          case TypeKind.NON_NULL => ""
          case TypeKind.LIST     => ""
          case TypeKind.UNION =>
            s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} = ${t.possibleTypes
              .flatMap(_.name)
              .mkString(" | ")}"""
          case _ =>
            s"""
               |${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} {
               |  ${t.fields.map(renderField).mkString("\n  ")}${t.enumValues.map(renderEnumValue).mkString("\n  ")}
               |}""".stripMargin
        }
    }.mkString("\n")

  def renderKind(kind: TypeKind): String =
    kind match {
      case TypeKind.OBJECT       => "type"
      case TypeKind.UNION        => "union"
      case TypeKind.ENUM         => "enum"
      case TypeKind.INPUT_OBJECT => "input"
      case _                     => ""
    }

  def renderDescription(description: Option[String]): String = description match {
    case None        => ""
    case Some(value) => if (value.contains("\n")) s"""\"\"\"\n$value\"\"\"\n""" else s""""$value"\n"""
  }

  def renderField(field: Field): String =
    s"${field.name}${renderArguments(field.args)}: ${renderTypeName(field.`type`())}${if (field.isDeprecated)
      s" @deprecated${field.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}"

  def renderEnumValue(v: EnumValue): String =
    s"${renderDescription(v.description)}${v.name}${if (v.isDeprecated)
      s" @deprecated${v.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}"

  def renderArguments(arguments: List[InputValue]): String = arguments match {
    case Nil  => ""
    case list => s"(${list.map(a => s"${a.name}: ${renderTypeName(a.`type`())}").mkString(", ")})"
  }

  def renderTypeName(fieldType: Type): String =
    fieldType.kind match {
      case TypeKind.NON_NULL => s"${fieldType.ofType.map(renderTypeName).getOrElse("null")}!"
      case TypeKind.LIST     => s"[${fieldType.ofType.map(renderTypeName).getOrElse("null")}]"
      case _                 => s"${fieldType.name.getOrElse("null")}"
    }
}
