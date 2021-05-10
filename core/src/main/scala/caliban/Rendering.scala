package caliban

import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import __TypeKind._

object Rendering {

  /**
   * Returns a string that renders the provided types into the GraphQL format.
   */
  def renderTypes(types: List[__Type]): String =
    types
      .sorted(typeOrdering)
      .flatMap { t =>
        t.kind match {
          case __TypeKind.SCALAR   =>
            t.name.flatMap(name =>
              if (isBuiltinScalar(name)) None
              else Some(s"""${renderDescription(t.description)}scalar $name""".stripMargin)
            )
          case __TypeKind.NON_NULL => None
          case __TypeKind.LIST     => None
          case __TypeKind.UNION    =>
            val renderedTypes: String =
              t.possibleTypes
                .fold(List.empty[String])(_.flatMap(_.name))
                .mkString(" | ")
            Some(
              s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)} = $renderedTypes"""
            )
          case _                   =>
            val renderedDirectives: String  = renderDirectives(t.directives)
            val renderedFields: String      = t
              .fields(__DeprecatedArgs(Some(true)))
              .fold(List.empty[String])(
                _.map(field =>
                  List(
                    field.description.map(_ => renderDescription(field.description)),
                    Some(renderField(field))
                  ).flatten
                    .mkString(("  "))
                )
              )
              .mkString("\n  ")
            val renderedInputFields: String = t.inputFields
              .fold(List.empty[String])(
                _.map(field =>
                  List(
                    field.description.map(_ => renderDescription(field.description)),
                    Some(renderInputValue(field))
                  ).flatten
                    .mkString(("  "))
                )
              )
              .mkString("\n  ")
            val renderedEnumValues          = t
              .enumValues(__DeprecatedArgs(Some(true)))
              .fold(List.empty[String])(_.map(renderEnumValue))
              .mkString("\n  ")
            Some(
              s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)}${renderInterfaces(t)}$renderedDirectives {
                 |  $renderedFields$renderedInputFields$renderedEnumValues
                 |}""".stripMargin
            )
        }
      }
      .mkString("\n\n")

  private def renderInterfaces(t: __Type): String =
    t.interfaces()
      .fold("")(_.flatMap(_.name) match {
        case Nil  => ""
        case list => s" implements ${list.mkString(" & ")}"
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

  private def renderDescription(description: Option[String], newline: Boolean = true): String = description match {
    case None                   => ""
    case Some(value) if newline => if (value.contains("\n")) s"""\"\"\"\n$value\"\"\"\n""" else s""""$value"\n"""
    case Some(value)            => if (value.contains("\n")) s"""\"\"\"$value\"\"\" """ else s""""$value" """
  }

  private def renderDirectiveArgument(value: InputValue): Option[String] = value match {
    case InputValue.ListValue(values)   =>
      Some(values.flatMap(renderDirectiveArgument).mkString("[", ",", "]"))
    case InputValue.ObjectValue(fields) =>
      Some(
        fields.map { case (key, value) => renderDirectiveArgument(value).map(v => s"$key: $v") }.mkString("{", ",", "}")
      )
    case NullValue                      => Some("null")
    case StringValue(value)             => Some("\"" + value + "\"")
    case i: IntValue                    => Some(i.toInt.toString)
    case f: FloatValue                  => Some(f.toFloat.toString)
    case BooleanValue(value)            => Some(value.toString)
    case EnumValue(value)               => Some(value)
    case InputValue.VariableValue(_)    => None
  }

  private def renderDirective(directive: Directive) =
    s"@${directive.name}${if (directive.arguments.nonEmpty) s"""(${directive.arguments.flatMap { case (key, value) =>
      renderDirectiveArgument(value).map(v => s"$key: $v")
    }.mkString(",")})"""
    else ""}"

  private def renderDirectives(directives: Option[List[Directive]]) =
    directives.fold("") {
      case Nil => ""
      case d   => d.map(renderDirective).mkString(" ", " ", "")
    }

  private def renderField(field: __Field): String =
    s"${field.name}${renderArguments(field.args)}: ${renderTypeName(field.`type`())}${if (field.isDeprecated)
      s" @deprecated${field.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}${renderDirectives(field.directives)}"

  private def renderInputValue(inputValue: __InputValue): String =
    s"${inputValue.name}: ${renderTypeName(inputValue.`type`())}${inputValue.defaultValue
      .fold("")(d => s" = $d")}${renderDirectives(inputValue.directives)}"

  private def renderEnumValue(v: __EnumValue): String =
    s"${renderDescription(v.description)}${v.name}${if (v.isDeprecated)
      s" @deprecated${v.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
    else ""}"

  private def renderArguments(arguments: List[__InputValue]): String = arguments match {
    case Nil  => ""
    case list =>
      s"(${list.map(a => s"${renderDescription(a.description, newline = false)}${a.name}: ${renderTypeName(a.`type`())}").mkString(", ")})"
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
