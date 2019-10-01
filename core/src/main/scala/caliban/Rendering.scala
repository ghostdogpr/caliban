package caliban

import caliban.introspection.adt._

object Rendering {

  /**
   * Returns a string that renders the provided types into the GraphQL format.
   */
  def renderTypes(types: Map[String, __Type]): String =
    types.flatMap {
      case (_, t) =>
        t.kind match {
          case __TypeKind.SCALAR   => None
          case __TypeKind.NON_NULL => None
          case __TypeKind.LIST     => None
          case __TypeKind.UNION =>
            Some(s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} = ${t.possibleTypes
              .getOrElse(Nil)
              .flatMap(_.name)
              .mkString(" | ")}""")
          case _ =>
            Some(s"""
                    |${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} {
                    |  ${t.fields(__DeprecatedArgs()).getOrElse(Nil).map(renderField).mkString("\n  ")}${t.inputFields
                      .getOrElse(Nil)
                      .map(renderInputValue)
                      .mkString("\n  ")}${t
                      .enumValues(__DeprecatedArgs())
                      .getOrElse(Nil)
                      .map(renderEnumValue)
                      .mkString("\n  ")}
                    |}""".stripMargin)
        }
    }.mkString("\n")

  private def renderKind(kind: __TypeKind): String =
    kind match {
      case __TypeKind.OBJECT       => "type"
      case __TypeKind.UNION        => "union"
      case __TypeKind.ENUM         => "enum"
      case __TypeKind.INPUT_OBJECT => "input"
      case _                       => ""
    }

  private def renderDescription(description: Option[String]): String = description match {
    case None        => ""
    case Some(value) => if (value.contains("\n")) s"""\"\"\"\n$value\"\"\"\n""" else s""""$value"\n"""
  }

  private def renderField(field: __Field): String =
    s"${field.name}${renderArguments(field.args)}: ${renderTypeName(field.`type`())}${if (field.isDeprecated)
      s" @deprecated${field.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}"

  private def renderInputValue(inputValue: __InputValue): String =
    s"${inputValue.name}: ${renderTypeName(inputValue.`type`())}${inputValue.defaultValue.fold("")(d => s" = $d")}"

  private def renderEnumValue(v: __EnumValue): String =
    s"${renderDescription(v.description)}${v.name}${if (v.isDeprecated)
      s" @deprecated${v.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}"

  private def renderArguments(arguments: List[__InputValue]): String = arguments match {
    case Nil  => ""
    case list => s"(${list.map(a => s"${a.name}: ${renderTypeName(a.`type`())}").mkString(", ")})"
  }

  private[caliban] def renderTypeName(fieldType: __Type): String =
    fieldType.kind match {
      case __TypeKind.NON_NULL => s"${fieldType.ofType.map(renderTypeName).getOrElse("null")}!"
      case __TypeKind.LIST     => s"[${fieldType.ofType.map(renderTypeName).getOrElse("null")}]"
      case _                   => s"${fieldType.name.getOrElse("null")}"
    }
}
