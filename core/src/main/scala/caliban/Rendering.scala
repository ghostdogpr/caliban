package caliban

import caliban.Value._
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.adt.Directive

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
              else
                Some(
                  s"""${renderDescription(t.description)}scalar $name${renderDirectives(
                      t.directives
                    )}${renderSpecifiedBy(t.specifiedBy)}""".stripMargin
                )
            )
          case __TypeKind.NON_NULL => None
          case __TypeKind.LIST     => None
          case __TypeKind.UNION    =>
            val renderedTypes: String =
              t.possibleTypes
                .fold(List.empty[String])(_.flatMap(_.name))
                .mkString(" | ")
            Some(
              s"""${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)}${renderDirectives(
                  t.directives
                )} = $renderedTypes"""
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
                    .mkString("  ")
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
                    .mkString("  ")
                )
              )
              .mkString("\n  ")
            val renderedEnumValues          = t
              .enumValues(__DeprecatedArgs(Some(true)))
              .fold(List.empty[String])(_.map(renderEnumValue))
              .mkString("\n  ")

            val typedef =
              s"${renderDescription(t.description)}${renderKind(t.kind)} ${renderTypeName(t)}${renderInterfaces(t)}$renderedDirectives"

            s"$renderedFields$renderedInputFields$renderedEnumValues" match {
              case ""       => Some(typedef)
              case interior =>
                Some(
                  s"""$typedef {
                     |  $interior
                     |}""".stripMargin
                )
            }
        }
      }
      .mkString("\n\n")

  def renderSchemaDirectives(directives: List[Directive]): String =
    directives
      .map(renderSchemaDirective)
      .mkString(" ")

  private def renderSchemaDirective(directive: Directive): String = {
    val args =
      if (directive.arguments.isEmpty) ""
      else directive.arguments.map { case (k, v) => s"$k: ${v.toInputString}" }.mkString("(", ", ", ") ")
    s"@${directive.name}$args"
  }

  def renderDirectives(directives: List[__Directive]): String =
    directives.map(renderDirective).mkString("\n")

  private def renderDirective(directive: __Directive): String = {
    val inputs             = directive.args match {
      case i if i.nonEmpty => s"""(${i.map(renderInputValue).mkString(", ")})"""
      case _               => ""
    }
    val locationStrings    = directive.locations.map {
      case __DirectiveLocation.QUERY                  => "QUERY"
      case __DirectiveLocation.MUTATION               => "MUTATION"
      case __DirectiveLocation.SUBSCRIPTION           => "SUBSCRIPTION"
      case __DirectiveLocation.FIELD                  => "FIELD"
      case __DirectiveLocation.FRAGMENT_DEFINITION    => "FRAGMENT_DEFINITION"
      case __DirectiveLocation.FRAGMENT_SPREAD        => "FRAGMENT_SPREAD"
      case __DirectiveLocation.INLINE_FRAGMENT        => "INLINE_FRAGMENT"
      case __DirectiveLocation.SCHEMA                 => "SCHEMA"
      case __DirectiveLocation.SCALAR                 => "SCALAR"
      case __DirectiveLocation.OBJECT                 => "OBJECT"
      case __DirectiveLocation.FIELD_DEFINITION       => "FIELD_DEFINITION"
      case __DirectiveLocation.ARGUMENT_DEFINITION    => "ARGUMENT_DEFINITION"
      case __DirectiveLocation.INTERFACE              => "INTERFACE"
      case __DirectiveLocation.UNION                  => "UNION"
      case __DirectiveLocation.ENUM                   => "ENUM"
      case __DirectiveLocation.ENUM_VALUE             => "ENUM_VALUE"
      case __DirectiveLocation.INPUT_OBJECT           => "INPUT_OBJECT"
      case __DirectiveLocation.INPUT_FIELD_DEFINITION => "INPUT_FIELD_DEFINITION"
      case __DirectiveLocation.VARIABLE_DEFINITION    => "VARIABLE_DEFINITION"
    }
    val directiveLocations = locationStrings.mkString(" | ")

    val on = if (directive.isRepeatable) { "repeatable on" }
    else { "on" }

    val body = s"""directive @${directive.name}${inputs} ${on} ${directiveLocations}""".stripMargin

    renderDescription(directive.description) match {
      case ""        => body
      case something => something + body
    }
  }

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

  private def renderDescription(description: Option[String], newline: Boolean = true): String = {
    // Most of the graphql tools are greedy on triple quotes. To be compatible we
    // need to break 4 or more '"' at the end of the description either with a newline or a space
    val tripleQuote = "\"\"\""

    def nlOrSp = if (newline) "\n" else " "

    def nlOrNot = if (newline) "\n" else ""

    def renderTripleQuotedString(value: String) = {
      val valueEscaped = value.replace(tripleQuote, s"\\$tripleQuote")
      // check if it ends in quote but it is already escaped
      if (value.endsWith("\\\""))
        s"$tripleQuote$nlOrNot$valueEscaped$nlOrNot$tripleQuote"
      // check if it ends in quote. We need to break the sequence of 4 or more '"'
      else if (value.last == '"') {
        s"$tripleQuote$nlOrNot$valueEscaped$nlOrSp$tripleQuote"
      } else {
        // ok. No quotes at the end of value
        s"$tripleQuote$nlOrNot$valueEscaped$nlOrNot$tripleQuote"
      }
    }
    description match {
      case None                                   => ""
      case Some(value) if value.exists(_ == '\n') =>
        s"${renderTripleQuotedString(s"$value")}$nlOrSp"
      case Some(value)                            => s"${renderString(value)}$nlOrSp"
    }
  }

  private def renderSpecifiedBy(specifiedBy: Option[String]): String =
    specifiedBy.fold("")(url => s""" @specifiedBy(url: "$url")""")

  private def renderDirectiveArgument(value: InputValue): Option[String] = value match {
    case InputValue.ListValue(values)   =>
      Some(values.flatMap(renderDirectiveArgument).mkString("[", ",", "]"))
    case InputValue.ObjectValue(fields) =>
      Some(
        fields.flatMap { case (key, value) => renderDirectiveArgument(value).map(v => s"$key: $v") }
          .mkString("{", ",", "}")
      )
    case NullValue                      => Some("null")
    case StringValue(value)             => Some(renderString(value))
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
      case d   => d.sortBy(_.name).map(renderDirective).mkString(" ", " ", "")
    }

  private def renderField(field: __Field): String =
    s"${field.name}${renderArguments(field.args)}: ${renderTypeName(field.`type`())}${if (field.isDeprecated)
        s" @deprecated${field.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
      else ""}${renderDirectives(field.directives)}"

  private def renderInputValue(inputValue: __InputValue): String =
    s"${inputValue.name}: ${renderTypeName(inputValue.`type`())}${renderDefaultValue(inputValue)}${renderDirectives(inputValue.directives)}"

  private def renderEnumValue(v: __EnumValue): String =
    s"${renderDescription(v.description)}${v.name}${if (v.isDeprecated)
        s" @deprecated${v.deprecationReason.fold("")(reason => s"""(reason: "$reason")""")}"
      else ""}${renderDirectives(v.directives)}"

  private def renderDefaultValue(a: __InputValue): String = a.defaultValue.fold("")(d => s" = $d")

  private def renderArguments(arguments: List[__InputValue]): String =
    arguments match {
      case Nil  => ""
      case list =>
        s"(${list.map(a => s"${renderDescription(a.description, newline = false)}${a.name}: ${renderTypeName(a.`type`())}${renderDefaultValue(a)}${renderDirectives(a.directives)}").mkString(", ")})"
    }

  private[caliban] def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

  private[caliban] def renderTypeName(fieldType: __Type): String = {
    lazy val renderedTypeName = fieldType.ofType.fold("null")(renderTypeName)
    fieldType.kind match {
      case __TypeKind.NON_NULL => renderedTypeName + "!"
      case __TypeKind.LIST     => s"[$renderedTypeName]"
      case _                   => s"${fieldType.name.getOrElse("null")}"
    }
  }

  private def renderString(value: String) =
    "\"" + value
      .replace("\\", "\\\\")
      .replace("\b", "\\b")
      .replace("\f", "\\f")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\"", "\\\"") + "\""
}
