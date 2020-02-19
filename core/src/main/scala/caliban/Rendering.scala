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
          case __TypeKind.SCALAR   => t.name.flatMap(name => if (isBuiltinScalar(name)) None else Some(s"scalar $name"))
          case __TypeKind.NON_NULL => None
          case __TypeKind.LIST     => None
          case __TypeKind.UNION =>
            val renderedTypes: String =
              t.possibleTypes
                .fold(List.empty[String])(_.flatMap(_.name))
                .mkString(" | ")
            Some(s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} = $renderedTypes""")
          case _ =>
            val renderedFields: String = t
              .fields(__DeprecatedArgs())
              .fold(List.empty[String])(_.map(renderField))
              .mkString("\n  ")
            val renderedInputFields: String = t.inputFields
              .fold(List.empty[String])(_.map(renderInputValue))
              .mkString("\n  ")
            val renderedEnumValues = t
              .enumValues(__DeprecatedArgs())
              .fold(List.empty[String])(_.map(renderEnumValue))
              .mkString("\n  ")
            Some(
              s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)}${renderInterfaces(t)} {
                 |  $renderedFields$renderedInputFields$renderedEnumValues
                 |}""".stripMargin
            )
        }
    }.mkString("\n\n")

  private def renderInterfaces(t: __Type): String =
    t.interfaces()
      .fold("")(_.flatMap(_.name) match {
        case Nil  => ""
        case list => s" implements ${list.mkString("& ")}"
      })

  private def renderKind(kind: __TypeKind): String =
    kind match {
      case __TypeKind.OBJECT       => "type"
      case __TypeKind.UNION        => "union"
      case __TypeKind.ENUM         => "enum"
      case __TypeKind.INPUT_OBJECT => "input"
      case __TypeKind.INTERFACE    => "interface"
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

  private def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

  private[caliban] def renderTypeName(fieldType: __Type): String = {
    lazy val renderedTypeName = fieldType.ofType.fold("null")(renderTypeName)
    fieldType.kind match {
      case __TypeKind.NON_NULL => renderedTypeName + "!"
      case __TypeKind.LIST     => s"[$renderedTypeName]"
      case _                   => s"${fieldType.name.getOrElse("null")}"
    }
  }
}
