package caliban

import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.introspection.adt._
import caliban.parsing.adt.Directive

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
            val renderedDirectives: String = renderDirectives(t.directives)
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
              s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)}${renderInterfaces(t)} $renderedDirectives {
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

  private def renderDirectiveArgument(value: InputValue): String = value match {
    case InputValue.ListValue(values) =>
      values.map(renderDirectiveArgument).mkString("[", ",", "]")
    case InputValue.ObjectValue(fields) =>
      fields.map { case (key, value) => s"$key: ${renderDirectiveArgument(value)}" }.mkString("{", ",", "}")
    case InputValue.VariableValue(_) =>
      throw new Exception("Variable values are not allowed in a directive declaration")
    case NullValue           => "null"
    case StringValue(value)  => "\"" + value + "\""
    case i: IntValue         => i.toInt.toString
    case f: FloatValue       => f.toFloat.toString
    case BooleanValue(value) => value.toString
    case EnumValue(value)    => value
  }

  private def renderDirective(directive: Directive) =
    s"@${directive.name}${if (directive.arguments.nonEmpty) s"""(${directive.arguments.map {
      case (key, value) => s"$key: ${renderDirectiveArgument(value)}"
    }.mkString("{", ",", "}")})"""
    else ""}"

  private def renderDirectives(directives: Option[List[Directive]]) =
    directives.fold("")(_.map(renderDirective).mkString(" ", " ", ""))

  private def renderField(field: __Field): String =
    s"${field.name}${renderArguments(field.args)}: ${renderTypeName(field.`type`())}${if (field.isDeprecated)
      s" @deprecated${field.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}${renderDirectives(field.directives)}"

  private def renderInputValue(inputValue: __InputValue): String =
    s"${inputValue.name}: ${renderTypeName(inputValue.`type`())}${inputValue.defaultValue.fold("")(d => s" = $d")}${renderDirectives(inputValue.directives)}"

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
