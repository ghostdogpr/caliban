package caliban.rendering

import caliban.InputValue
import caliban.introspection.adt.{ __DirectiveLocation, __Type }
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation.{
  ExecutableDirectiveLocation,
  TypeSystemDirectiveLocation
}
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{
  DirectiveDefinition,
  DirectiveLocation,
  SchemaDefinition,
  TypeDefinition
}
import caliban.parsing.adt.Type.{ innerType, NamedType }
import caliban.parsing.adt._

import scala.annotation.switch

object DocumentRenderer extends Renderer[Document] {
  override def render(document: Document): String = {
    val builder = new StringBuilder(64)
    unsafeRender(document, Some(0), builder)
    builder.toString()
  }

  override def renderCompact(document: Document): String = {
    val builder = new StringBuilder
    unsafeRender(document, None, builder)
    builder.toString()
  }

  private implicit val typeOrdering: Ordering[TypeDefinition] = Ordering.by {
    case TypeDefinition.ScalarTypeDefinition(_, name, _)          => (0, name)
    case TypeDefinition.UnionTypeDefinition(_, name, _, _)        => (1, name)
    case TypeDefinition.EnumTypeDefinition(_, name, _, _)         => (2, name)
    case TypeDefinition.InputObjectTypeDefinition(_, name, _, _)  => (3, name)
    case TypeDefinition.InterfaceTypeDefinition(_, name, _, _, _) => (4, name)
    case TypeDefinition.ObjectTypeDefinition(_, name, _, _, _)    => (5, name)
  }

  override protected[caliban] def unsafeRender(value: Document, indent: Option[Int], write: StringBuilder): Unit =
    documentRenderer.unsafeRender(value, indent, write)

  private[caliban] lazy val documentRenderer: Renderer[Document] = Renderer.combine(
    directiveDefinitionRenderer.list("\n").contramap(_.directiveDefinitions),
    schemaRenderer.optional.contramap(_.schemaDefinition),
    operationDefinitionRenderer.list.contramap(_.operationDefinitions),
    typeDefinitionsRenderer.contramap(_.typeDefinitions),
    fragmentRenderer.list.contramap(_.fragmentDefinitions)
  )

  private lazy val directiveDefinitionRenderer: Renderer[DirectiveDefinition] =
    new Renderer[DirectiveDefinition] {
      override def unsafeRender(value: DirectiveDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case DirectiveDefinition(description, name, args, isRepeatable, locations) =>
            unsafeRenderDescription(description, indent, write)
            write ++= "directive @"
            write ++= name
            if (args.nonEmpty) {
              write += '('
              args.foreach(inputValueDefinitionRenderer.unsafeRender(_, indent, write))
              write += ')'
            }
            if (isRepeatable) write ++= " repeatable"
            write ++= " on"
            var first = true
            locations.foreach { location =>
              write += ' '
              if (first) first = false
              else write ++= "| "
              unsafeRenderLocation(location, write)
            }
        }

      private def unsafeRenderLocation(location: DirectiveLocation, builder: StringBuilder): Unit =
        location match {
          case ExecutableDirectiveLocation.QUERY                  => builder ++= "QUERY"
          case ExecutableDirectiveLocation.MUTATION               => builder ++= "MUTATION"
          case ExecutableDirectiveLocation.SUBSCRIPTION           => builder ++= "SUBSCRIPTION"
          case ExecutableDirectiveLocation.FIELD                  => builder ++= "FIELD"
          case ExecutableDirectiveLocation.FRAGMENT_DEFINITION    => builder ++= "FRAGMENT_DEFINITION"
          case ExecutableDirectiveLocation.FRAGMENT_SPREAD        => builder ++= "FRAGMENT_SPREAD"
          case ExecutableDirectiveLocation.INLINE_FRAGMENT        => builder ++= "INLINE_FRAGMENT"
          case TypeSystemDirectiveLocation.SCHEMA                 => builder ++= "SCHEMA"
          case TypeSystemDirectiveLocation.SCALAR                 => builder ++= "SCALAR"
          case TypeSystemDirectiveLocation.OBJECT                 => builder ++= "OBJECT"
          case TypeSystemDirectiveLocation.FIELD_DEFINITION       => builder ++= "FIELD_DEFINITION"
          case TypeSystemDirectiveLocation.ARGUMENT_DEFINITION    => builder ++= "ARGUMENT_DEFINITION"
          case TypeSystemDirectiveLocation.INTERFACE              => builder ++= "INTERFACE"
          case TypeSystemDirectiveLocation.UNION                  => builder ++= "UNION"
          case TypeSystemDirectiveLocation.ENUM                   => builder ++= "ENUM"
          case TypeSystemDirectiveLocation.ENUM_VALUE             => builder ++= "ENUM_VALUE"
          case TypeSystemDirectiveLocation.INPUT_OBJECT           => builder ++= "INPUT_OBJECT"
          case TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION => builder ++= "INPUT_FIELD_DEFINITION"
          case TypeSystemDirectiveLocation.VARIABLE_DEFINITION    => builder ++= "VARIABLE_DEFINITION"
        }
    }

  private[caliban] lazy val typesRenderer: Renderer[List[__Type]] =
    typeDefinitionsRenderer.contramap(_.flatMap(_.toTypeDefinition))

  private[caliban] lazy val operationDefinitionRenderer: Renderer[OperationDefinition] =
    new Renderer[OperationDefinition] {
      override def unsafeRender(definition: OperationDefinition, indent: Option[Int], writer: StringBuilder): Unit =
        definition match {
          case OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet) =>
            newlineOrSpace(indent, writer)
            operationTypeRenderer.unsafeRender(operationType, indent, writer)
            name.foreach { n =>
              writer += ' '
              writer ++= n
            }
            variableDefinitionsRenderer.unsafeRender(variableDefinitions, indent, writer)
            directivesRenderer.unsafeRender(directives, indent, writer)
            selectionRenderer.unsafeRender(selectionSet, indent, writer)
        }
    }

  private[caliban] lazy val operationTypeRenderer: Renderer[OperationType] = new Renderer[OperationType] {
    override def unsafeRender(operationType: OperationType, indent: Option[Int], write: StringBuilder): Unit =
      operationType match {
        case OperationType.Query        => write ++= "query"
        case OperationType.Mutation     => write ++= "mutation"
        case OperationType.Subscription => write ++= "subscription"
      }
  }

  private[caliban] lazy val variableDefinition: Renderer[VariableDefinition] = new Renderer[VariableDefinition] {
    override def unsafeRender(definition: VariableDefinition, indent: Option[Int], builder: StringBuilder): Unit = {
      builder += '$'
      builder ++= definition.name
      builder += ':'
      space(indent, builder)
      builder ++= definition.variableType.toString
      definition.defaultValue.foreach { value =>
        space(indent, builder)
        builder += '='
        space(indent, builder)
        builder ++= value.toInputString
      }
    }
  }

  private[caliban] lazy val variableDefinitionsRenderer: Renderer[List[VariableDefinition]] =
    new Renderer[List[VariableDefinition]] {
      private val inner = variableDefinition.list(", ")

      override def unsafeRender(value: List[VariableDefinition], indent: Option[Int], write: StringBuilder): Unit =
        if (value.nonEmpty) {
          write += '('
          inner.unsafeRender(value, indent, write)
          write += ')'
        }
    }

  private[caliban] lazy val selectionRenderer: Renderer[List[Selection]] = new Renderer[List[Selection]] {
    override def unsafeRender(selections: List[Selection], indent: Option[Int], builder: StringBuilder): Unit = {
      space(indent, builder)
      builder += '{'
      selections.foreach(loop(_, increment(indent), builder))
      newline(indent, builder)
      pad(indent, builder)
      builder += '}'

    }

    private def loop(selection: Selection, indent: Option[Int], builder: StringBuilder): Unit =
      selection match {
        case Selection.Field(alias, name, arguments, directives, selectionSet, _) =>
          builder += '\n'
          pad(indent, builder)
          alias.foreach { a =>
            builder ++= a
            builder += ':'
            builder += ' '
          }
          builder ++= name
          inputArgumentsRenderer.unsafeRender(arguments, indent, builder)
          directivesRenderer.unsafeRender(directives, indent, builder)
          if (selectionSet.nonEmpty) {
            space(indent, builder)
            builder += '{'
            selectionSet.foreach(loop(_, increment(indent), builder))
            newline(indent, builder)
            pad(indent, builder)
            builder += '}'
          }
        case Selection.FragmentSpread(name, directives)                           =>
          newlineOrSpace(indent, builder)
          pad(indent, builder)
          builder ++= "..."
          builder ++= name
          directivesRenderer.unsafeRender(directives, indent, builder)
        case Selection.InlineFragment(typeCondition, dirs, selectionSet)          =>
          newlineOrSpace(indent, builder)
          pad(indent, builder)
          builder ++= "..."
          typeCondition.foreach { t =>
            builder ++= " on "
            builder ++= t.name
          }
          directivesRenderer.unsafeRender(dirs, indent, builder)
          if (selectionSet.nonEmpty) {
            space(indent, builder)
            builder += '{'
            selectionSet.foreach(loop(_, increment(indent), builder))
            newline(indent, builder)
            pad(indent, builder)
            builder += '}'
          }
      }
  }

  private lazy val inputArgumentsRenderer: Renderer[Map[String, InputValue]] = new Renderer[Map[String, InputValue]] {
    override def unsafeRender(arguments: Map[String, InputValue], indent: Option[Int], builder: StringBuilder): Unit =
      if (arguments.nonEmpty) {
        builder += '('
        var first = true
        arguments.foreach { case (name, value) =>
          if (first) first = false
          else {
            builder += ','
            space(indent, builder)
          }
          builder ++= name
          builder += ':'
          space(indent, builder)
          builder ++= value.toInputString
        }
        builder += ')'
      }
  }

  lazy val schemaRenderer: Renderer[SchemaDefinition] = new Renderer[SchemaDefinition] {
    override def unsafeRender(definition: SchemaDefinition, indent: Option[Int], write: StringBuilder): Unit =
      definition match {
        case SchemaDefinition(directives, query, mutation, subscription, description) =>
          val hasTypes    = query.nonEmpty || mutation.nonEmpty || subscription.nonEmpty
          val isExtension = directives.nonEmpty && !hasTypes
          var first       = true

          def renderOp(name: String, op: Option[String]): Unit =
            op.foreach { o =>
              if (first) {
                newline(indent, write)
                first = false
              } else newlineOrComma(indent, write)
              pad(increment(indent), write)
              write ++= name
              write += ':'
              space(indent, write)
              write ++= o
            }

          unsafeRenderDescription(description, indent, write)
          if (isExtension) write ++= "extend "
          if (isExtension || hasTypes) {
            write ++= "schema"
            directivesRenderer.unsafeRender(directives, indent, write)
            if (hasTypes) {
              space(indent, write)
              write += '{'
              renderOp("query", query)
              renderOp("mutation", mutation)
              renderOp("subscription", subscription)
              newline(indent, write)
              write += '}'
            }
          }
      }
  }

  private[caliban] def typeDefinitionsRenderer: Renderer[List[TypeDefinition]] =
    typeDefinitionRenderer.list.contramap(_.sorted)

  private[caliban] lazy val typeDefinitionRenderer: Renderer[TypeDefinition] = new Renderer[TypeDefinition] {
    override def unsafeRender(definition: TypeDefinition, indent: Option[Int], writer: StringBuilder): Unit =
      definition match {
        case typ: TypeDefinition.ObjectTypeDefinition      =>
          objectTypeDefinitionRenderer.unsafeRender(typ, indent, writer)
        case typ: TypeDefinition.InterfaceTypeDefinition   =>
          interfaceTypeDefinitionRenderer.unsafeRender(typ, indent, writer)
        case typ: TypeDefinition.InputObjectTypeDefinition =>
          inputObjectTypeDefinition.unsafeRender(typ, indent, writer)
        case typ: TypeDefinition.EnumTypeDefinition        =>
          enumRenderer.unsafeRender(typ, indent, writer)
        case typ: TypeDefinition.UnionTypeDefinition       =>
          unionRenderer.unsafeRender(typ, indent, writer)
        case typ: TypeDefinition.ScalarTypeDefinition      =>
          scalarRenderer.unsafeRender(typ, indent, writer)
      }
  }

  private[caliban] lazy val fragmentRenderer: Renderer[FragmentDefinition] = new Renderer[FragmentDefinition] {
    override def unsafeRender(value: FragmentDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case FragmentDefinition(name, typeCondition, directives, selectionSet) =>
          newlineOrSpace(indent, write)
          write ++= "fragment "
          write ++= name
          write ++= " on "
          write ++= typeCondition.name
          directivesRenderer.unsafeRender(directives, indent, write)
          selectionRenderer.unsafeRender(selectionSet, indent, write)
      }
  }

  private[caliban] lazy val unionRenderer: Renderer[UnionTypeDefinition] = new Renderer[UnionTypeDefinition] {
    override def unsafeRender(value: UnionTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case UnionTypeDefinition(description, name, directives, members) =>
          newlineOrSpace(indent, write)
          unsafeRenderDescription(description, indent, write)
          write ++= "union "
          write ++= name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write += '='
          space(indent, write)
          var first = true
          members.foreach { member =>
            if (first) first = false
            else {
              space(indent, write)
              write += '|'
              space(indent, write)
            }
            write ++= member
          }
      }
  }

  private[caliban] lazy val scalarRenderer: Renderer[ScalarTypeDefinition] = new Renderer[ScalarTypeDefinition] {
    override def unsafeRender(value: ScalarTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case ScalarTypeDefinition(description, name, directives) =>
          if (!isBuiltinScalar(name)) {
            newlineOrSpace(indent, write)
            unsafeRenderDescription(description, indent, write)
            write ++= "scalar "
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
          }
      }
  }

  private[caliban] lazy val enumRenderer: Renderer[EnumTypeDefinition] = new Renderer[EnumTypeDefinition] {
    override def unsafeRender(value: EnumTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case EnumTypeDefinition(description, name, directives, values) =>
          newlineOrSpace(indent, write)
          unsafeRenderDescription(description, indent, write)
          write ++= "enum "
          write ++= name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write += '{'
          values.foreach(enumValueDefinitionRenderer.unsafeRender(_, increment(indent), write))
          write += '}'
      }
  }

  private[caliban] lazy val enumValueDefinitionRenderer: Renderer[EnumValueDefinition] =
    new Renderer[EnumValueDefinition] {
      override def unsafeRender(value: EnumValueDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case EnumValueDefinition(description, name, directives) =>
            newlineOrComma(indent, write)
            unsafeRenderDescription(description, indent, write)
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
        }
    }

  private[caliban] lazy val inputObjectTypeDefinition: Renderer[InputObjectTypeDefinition] =
    new Renderer[InputObjectTypeDefinition] {
      override def unsafeRender(value: InputObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InputObjectTypeDefinition(description, name, directives, fields) =>
            newlineOrSpace(indent, write)
            unsafeRenderDescription(description, indent, write)
            write ++= "input "
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
            space(indent, write)
            write += '{'
            fields.foreach(inputValueDefinitionRenderer.unsafeRender(_, increment(indent), write))
            newline(indent, write)
            write += '}'
            newline(indent, write)
        }
    }

  private[caliban] lazy val inputValueDefinitionRenderer: Renderer[InputValueDefinition] =
    new Renderer[InputValueDefinition] {
      override def unsafeRender(
        definition: InputValueDefinition,
        indent: Option[Int],
        builder: StringBuilder
      ): Unit =
        definition match {
          case InputValueDefinition(description, name, valueType, defaultValue, directives) =>
            newlineOrSpace(indent, builder)
            unsafeRenderDescription(description, indent, builder)
            pad(indent, builder)
            builder ++= name
            builder += ':'
            space(indent, builder)
            builder ++= valueType.toString
            defaultValue.foreach { value =>
              space(indent, builder)
              builder += '='
              space(indent, builder)
              builder ++= value.toInputString
            }
            directivesRenderer.unsafeRender(directives, indent, builder)
        }
    }

  private[caliban] lazy val objectTypeDefinitionRenderer: Renderer[ObjectTypeDefinition] =
    new Renderer[ObjectTypeDefinition] {
      override def unsafeRender(value: ObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case ObjectTypeDefinition(description, name, interfaces, directives, fields) =>
            unsafeRenderObjectLike("type", description, name, interfaces, directives, fields, indent, write)
        }
    }

  private[caliban] lazy val interfaceTypeDefinitionRenderer: Renderer[InterfaceTypeDefinition] =
    new Renderer[InterfaceTypeDefinition] {
      override def unsafeRender(value: InterfaceTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InterfaceTypeDefinition(description, name, implements, directives, fields) =>
            unsafeRenderObjectLike("interface", description, name, implements, directives, fields, indent, write)
        }
    }

  private def unsafeRenderObjectLike(
    variant: String,
    description: Option[String],
    name: String,
    implements: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition],
    indent: Option[Int],
    writer: StringBuilder
  ): Unit = {
    newline(indent, writer)
    newline(indent, writer)
    unsafeRenderDescription(description, None, writer)
    writer ++= variant
    writer += ' '
    writer ++= name
    implements match {
      case Nil          =>
      case head :: tail =>
        writer ++= " implements "
        writer ++= innerType(head)
        tail.foreach { impl =>
          writer ++= " & "
          writer ++= innerType(impl)
        }
    }
    directivesRenderer.unsafeRender(directives, indent, writer)
    if (fields.nonEmpty) {
      space(indent, writer)
      writer += '{'
      fields.foreach(fieldDefinitionRenderer.unsafeRender(_, increment(indent), writer))
      newline(indent, writer)
      writer += '}'
    }
  }

  private lazy val directiveRenderer: Renderer[Directive] = new Renderer[Directive] {
    override def unsafeRender(d: Directive, indent: Option[Int], writer: StringBuilder): Unit = {
      space(indent, writer)
      writer += '@'
      writer ++= d.name
      var first = true
      if (d.arguments.nonEmpty) {
        writer += '('
        d.arguments.foreach { case (name, value) =>
          if (first) {
            writer ++= name
            writer += ':'
            space(indent, writer)
            writer ++= value.toInputString
            first = false
          } else {
            writer += ','
            space(indent, writer)
            writer ++= name
            writer += ':'
            space(indent, writer)
            writer ++= value.toInputString
          }
        }
        writer += ')'
      }

    }
  }

  private[caliban] lazy val directivesRenderer: Renderer[List[Directive]] =
    directiveRenderer.list

  private def unsafeRenderDescription(
    description: Option[String],
    indent: Option[Int],
    writer: StringBuilder
  ): Unit = {
    val tripleQuote = "\"\"\""

    description.foreach {
      case value if value.contains('\n') =>
        def valueEscaped() = unsafeFastEscapeQuote(value, writer)
        writer ++= tripleQuote
        // check if it ends in quote but it is already escaped
        if (value.endsWith("\\\"")) {
          newline(indent, writer)
          valueEscaped()
          newline(indent, writer)
        } else if (value.last == '"') {
          newline(indent, writer)
          valueEscaped()
          newlineOrSpace(indent, writer)
          // check if it ends in quote. We need to break the sequence of 4 or more '"'
        } else {
          // ok. No quotes at the end of value
          newline(indent, writer)
          valueEscaped()
          newline(indent, writer)
        }
        writer ++= tripleQuote
      case value                         =>
        pad(indent, writer)
        unsafeFastEscape(value, writer)
        newlineOrSpace(indent, writer)
    }
  }

  private[caliban] lazy val fieldDefinitionRenderer: Renderer[FieldDefinition] = new Renderer[FieldDefinition] {
    override def unsafeRender(definition: FieldDefinition, indent: Option[Int], builder: StringBuilder): Unit =
      definition match {
        case FieldDefinition(description, name, arguments, tpe, directives) =>
          newlineOrSpace(indent, builder)
          unsafeRenderDescription(description, indent, builder)
          pad(indent, builder)
          builder ++= name
          unsafeRenderArguments(arguments, indent, builder)
          builder += ':'
          space(indent, builder)
          builder ++= tpe.toString

          directivesRenderer.unsafeRender(directives, None, builder)
      }
  }

  private[caliban] def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

  private def unsafeRenderArguments(
    definitions: List[InputValueDefinition],
    indent: Option[Int],
    builder: StringBuilder
  ): Unit =
    if (definitions.nonEmpty) {
      builder += '('
      var first = true
      definitions.foreach { definition =>
        if (first) first = false
        else {
          builder += ','
          space(indent, builder)
        }
        inlineInputValueDefinitionRenderer.unsafeRender(definition, indent, builder)
      }
      builder += ')'
    }

  private[caliban] lazy val inlineInputValueDefinitionRenderer: Renderer[InputValueDefinition] =
    new Renderer[InputValueDefinition] {
      override def unsafeRender(definition: InputValueDefinition, indent: Option[Int], builder: StringBuilder): Unit =
        definition match {
          case InputValueDefinition(description, name, tpe, defaultValue, directives) =>
            unsafeRenderDescription(description, None, builder)
            builder ++= name
            builder += ':'
            space(indent, builder)
            builder ++= tpe.toString
            defaultValue.foreach { value =>
              space(indent, builder)
              builder += '='
              space(indent, builder)
              builder ++= value.toInputString
            }
            directivesRenderer.unsafeRender(directives, None, builder)
        }
    }

  private def pad(indentation: Option[Int], writer: StringBuilder): Unit = {
    var i = indentation.getOrElse(0)
    while (i > 0) {
      writer ++= "  "
      i -= 1
    }
  }

  private def space(indentation: Option[Int], writer: StringBuilder): Unit =
    if (indentation.isDefined) writer += ' '

  private def newline(indent: Option[Int], writer: StringBuilder): Unit =
    if (indent.isDefined) writer += '\n'

  private def newlineOrSpace(indent: Option[Int], writer: StringBuilder): Unit =
    if (indent.isDefined) writer += '\n' else writer += ' '

  private def newlineOrComma(indentation: Option[Int], writer: StringBuilder): Unit =
    if (indentation.isDefined) writer += '\n' else writer += ','

  private def increment(indentation: Option[Int]): Option[Int] = indentation.map(_ + 1)

  private def unsafeFastEscape(value: String, builder: StringBuilder) = {
    builder.append('"')
    var i = 0
    while (i < value.length) {
      (value.charAt(i): @switch) match {
        case '\\' => builder.append("\\\\")
        case '\b' => builder.append("\\b")
        case '\f' => builder.append("\\f")
        case '\n' => builder.append("\\n")
        case '\r' => builder.append("\\r")
        case '\t' => builder.append("\\t")
        case '"'  => builder.append("\\\"")
        case c    => builder.append(c)
      }
      i += 1
    }
    builder.append('"')
  }

  /**
   * A zero allocation version of triple quote escaping.
   */
  private def unsafeFastEscapeQuote(value: String, builder: StringBuilder): Unit = {
    var i                 = 0
    var quotes            = 0
    def padQuotes(): Unit =
      while (quotes > 0) {
        builder.append('"')
        quotes -= 1
      }
    while (i < value.length) {
      (value.charAt(i): @switch) match {
        case '"' =>
          quotes += 1
          // We have encountered a triple quote sequence
          if (quotes == 3) {
            builder.append("\\")
            padQuotes()
          }
        case c   =>
          // We have encountered a non-quote character before reaching the end of the triple sequence
          if (quotes > 0) {
            padQuotes()
          }
          builder.append(c)
      }
      i += 1
    }
    // If we reached the end without fully closing the triple quote sequence, we need to append the buffer to the builder
    if (quotes > 0) {
      padQuotes()
    }
  }

}
