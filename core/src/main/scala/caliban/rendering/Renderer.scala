package caliban.rendering

import caliban.{ InputValue, Rendering }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{
  EnumValueDefinition,
  FieldDefinition,
  InputValueDefinition
}
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Type.{ innerType, NamedType }
import caliban.parsing.adt.{ Definition, Directive, Document, OperationType, Selection, VariableDefinition }

object Renderer {

  private implicit val typeOrdering: Ordering[TypeDefinition] = Ordering.by {
    case TypeDefinition.ScalarTypeDefinition(_, name, _)          => (0, name)
    case TypeDefinition.UnionTypeDefinition(_, name, _, _)        => (1, name)
    case TypeDefinition.EnumTypeDefinition(_, name, _, _)         => (2, name)
    case TypeDefinition.InputObjectTypeDefinition(_, name, _, _)  => (3, name)
    case TypeDefinition.InterfaceTypeDefinition(_, name, _, _, _) => (4, name)
    case TypeDefinition.ObjectTypeDefinition(_, name, _, _, _)    => (5, name)
  }

  def render(document: Document): String = {
    val builder = new StringBuilder
    unsafeRender(document, builder)
    builder.toString()
  }

  private[caliban] def unsafeRender(document: Document, writer: StringBuilder): Unit = {
    val maybeSchema     = document.schemaDefinition
    val maybeOperations = document.operationDefinitions
    maybeSchema.foreach(unsafeRenderSchema(_, writer))
    maybeOperations.foreach(unsafeRenderOperation(_, writer))
    unsafeRenderTypeDefinitions(document.typeDefinitions, writer)
  }

  private def unsafeRenderTypeDefinitions(definitions: List[TypeDefinition], writer: StringBuilder): Unit =
    if (definitions.nonEmpty) {
      definitions.sorted.foreach { definition =>
        unsafeRenderType(definition, writer)
      }
    }

  private def unsafeRenderOperation(definition: OperationDefinition, writer: StringBuilder): Unit = definition match {
    case OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet) =>
      unsafeRenderOperationType(operationType, writer)
      name.foreach { n =>
        writer += ' '
        writer ++= n
      }
      unsafeRenderVariableDefinitions(variableDefinitions, writer)
      unsafeRenderDirectives(directives, writer)
      unsafeRenderSelectionSets(selectionSet, writer)
  }

  private def unsafeRenderOperationType(operationType: OperationType, writer: StringBuilder): Unit =
    operationType match {
      case OperationType.Query        => writer ++= "query"
      case OperationType.Mutation     => writer ++= "mutation"
      case OperationType.Subscription => writer ++= "subscription"
    }

  private def unsafeRenderVariableDefinitions(definitions: List[VariableDefinition], builder: StringBuilder): Unit =
    if (definitions.nonEmpty) {
      builder += '('
      var first = true
      definitions.foreach { definition =>
        if (first) first = false
        else builder ++= ", "
        builder += '$'
        builder ++= definition.name
        builder ++= ": "
        builder ++= definition.variableType.toString
        definition.defaultValue.foreach { value =>
          builder ++= " = "
          builder ++= value.toInputString
        }
      }
      builder += ')'
    }

  private def unsafeRenderSelectionSets(selections: List[Selection], builder: StringBuilder): Unit = if (
    selections.nonEmpty
  ) {
    builder += '{'
    builder += '\n'
    selections.foreach(unsafeRenderSelection(_, builder))
    builder += '}'
  }

  private def unsafeRenderSelection(selection: Selection, builder: StringBuilder): Unit = selection match {
    case Selection.Field(alias, name, arguments, directives, selectionSet, _) =>
      alias.foreach { a =>
        builder ++= a
        builder += ':'
        builder += ' '
      }
      builder ++= name
      unsafeRenderInputArguments(arguments, builder)
      unsafeRenderDirectives(directives, builder)
      selectionSet.foreach { child =>
        builder += '{'
        unsafeRenderSelection(child, builder)
        builder += '}'
      }
    case Selection.FragmentSpread(name, directives)                           =>
      builder ++= "..."
      builder ++= name
      unsafeRenderDirectives(directives, builder)
      builder += '\n'
    case Selection.InlineFragment(typeCondition, dirs, selectionSet)          =>
      builder ++= "..."
      typeCondition.foreach { t =>
        builder ++= " on "
        builder ++= t.name
      }
      unsafeRenderDirectives(dirs, builder)
      unsafeRenderSelectionSets(selectionSet, builder)
  }

  private def unsafeRenderInputArguments(arguments: Map[String, InputValue], builder: StringBuilder): Unit =
    if (arguments.nonEmpty) {
      builder += '('
      var first = true
      arguments.foreach { case (name, value) =>
        if (first) first = false
        else builder ++= ", "
        builder ++= name
        builder ++= ": "
        builder ++= value.toInputString
      }
      builder += ')'
    }

  private def unsafeRenderType(definition: TypeDefinition, writer: StringBuilder): Unit = definition match {
    case TypeDefinition.ObjectTypeDefinition(description, name, implements, directives, fields)    =>
      unsafeRenderObjectLike("type", description, name, implements, directives, fields, writer)
    case TypeDefinition.InterfaceTypeDefinition(description, name, implements, directives, fields) =>
      unsafeRenderObjectLike("interface", description, name, implements, directives, fields, writer)
    case TypeDefinition.InputObjectTypeDefinition(description, name, directives, fields)           =>
      unsafeRenderInputObject(description, name, directives, fields, writer)
    case TypeDefinition.EnumTypeDefinition(description, name, directives, enumValuesDefinition)    =>
      unsafeRenderEnum(description, name, directives, enumValuesDefinition, writer)
    case TypeDefinition.UnionTypeDefinition(description, name, directives, memberTypes)            =>
      unsafeRenderUnion(description, name, directives, memberTypes, writer)
    case TypeDefinition.ScalarTypeDefinition(description, name, directives)                        =>
      unsafeRenderScalar(description, name, directives, writer)
  }

  private def unsafeRenderSchema(definition: SchemaDefinition, writer: StringBuilder): Unit = definition match {
    case SchemaDefinition(directives, query, mutation, subscription, description) =>
      val isExtension = directives.nonEmpty && query.isEmpty && mutation.isEmpty && subscription.isEmpty

      unsafeRenderDescription(description, writer)
      if (isExtension) writer ++= "extend "
      writer ++= "schema"
      unsafeRenderDirectives(directives, writer)
      if (!isExtension) {
        writer ++= " {"
        query.foreach(q => writer ++= s"\n  query: $q")
        mutation.foreach(m => writer ++= s"\n  mutation: $m")
        subscription.foreach(s => writer ++= s"\n  subscription: $s")
        writer ++= "\n}\n"
      } else {
        writer += '\n'
      }
  }

  private def unsafeRenderUnion(
    description: Option[String],
    name: String,
    directives: List[Directive],
    members: List[String],
    builder: StringBuilder
  ): Unit = {
    builder += '\n'
    unsafeRenderDescription(description, builder)
    builder ++= "union "
    builder ++= name
    unsafeRenderDirectives(directives, builder)
    builder ++= " = "
    var first = true
    members.foreach { member =>
      if (first) first = false
      else builder ++= " | "
      builder ++= member
    }
  }

  private def unsafeRenderScalar(
    description: Option[String],
    name: String,
    directives: List[Directive],
    builder: StringBuilder
  ): Unit =
    if (!Rendering.isBuiltinScalar(name)) {
      builder += '\n'
      unsafeRenderDescription(description, builder)
      builder ++= "scalar "
      builder ++= name
      unsafeRenderDirectives(directives, builder)
    }

  private def unsafeRenderEnum(
    description: Option[String],
    name: String,
    directives: List[Directive],
    definitions: List[EnumValueDefinition],
    builder: StringBuilder
  ): Unit = {
    builder += '\n'
    unsafeRenderDescription(description, builder)
    builder ++= "enum "
    builder ++= name
    unsafeRenderDirectives(directives, builder)
    builder += '{'
    definitions.foreach { definition =>
      unsafeRenderEnumValueDefinition(definition, builder)
      builder += '\n'
    }
    builder += '}'
  }

  private def unsafeRenderEnumValueDefinition(definition: EnumValueDefinition, builder: StringBuilder): Unit =
    definition match {
      case EnumValueDefinition(description, name, directives) =>
        unsafeRenderDescription(description, builder)
        builder ++= name
        unsafeRenderDirectives(directives, builder)
    }

  private def unsafeRenderInputObject(
    description: Option[String],
    name: String,
    directives: List[Directive],
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition],
    builder: StringBuilder
  ): Unit = {
    builder += '\n'
    unsafeRenderDescription(description, builder)
    builder ++= "input "
    builder ++= name
    unsafeRenderDirectives(directives, builder)
    builder += '{'
    definitions.foreach { definition =>
      builder += '\n'
      unsafeRenderInputValueDefinition(definition, builder)
    }
    builder += '\n'
    builder += '}'
    builder += '\n'
  }

  private def unsafeRenderInputValueDefinition(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition,
    builder: StringBuilder
  ): Unit = definition match {
    case InputValueDefinition(description, name, valueType, defaultValue, directives) =>
      unsafeRenderDescription(description, builder)
      builder ++= name
      builder ++= ": "
      builder ++= valueType.toString
      defaultValue.foreach { value =>
        builder ++= " = "
        builder ++= value.toInputString
      }
      unsafeRenderDirectives(directives, builder)
  }

  private def unsafeRenderObjectLike(
    variant: String,
    description: Option[String],
    name: String,
    implements: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition],
    writer: StringBuilder
  ): Unit = {
    writer += '\n'
    unsafeRenderDescription(description, writer)
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
    unsafeRenderDirectives(directives, writer)
    writer += ' '
    writer += '{'
    fields.foreach { field =>
      writer += '\n'
      unsafeRenderField(field, writer)
    }
    writer += '\n'
    writer += '}'
  }

  private def unsafeRenderDirectives(directives: List[Directive], writer: StringBuilder): Unit =
    directives.foreach { d =>
      writer ++= " @"
      writer ++= d.name
      var first = true
      d.arguments.foreach { case (name, value) =>
        if (first) {
          writer += '('
          writer ++= name
          writer ++= ": "
          writer ++= value.toInputString
          first = false
        } else {
          writer ++= ", "
          writer ++= name
          writer ++= ": "
          writer ++= value.toInputString
        }
      }
      if (!first) writer += ')'
    }

  private def unsafeRenderDescription(
    description: Option[String],
    writer: StringBuilder,
    newline: Boolean = true
  ): Unit = {
    val tripleQuote = "\"\"\""

    description.foreach {
      case value if value.contains('\n') =>
        val valueEscaped = value.replace(tripleQuote, s"\\$tripleQuote")
        if (value.endsWith("\\\"")) {
          writer ++= tripleQuote
          writer ++= valueEscaped
          if (newline) writer += '\n'
          writer ++= tripleQuote
        } else if (value.last == '"') {
          writer ++= tripleQuote
          if (newline) writer += '\n'
          writer ++= valueEscaped
          if (newline) writer += '\n'
          writer ++= tripleQuote
        } else {
          writer ++= tripleQuote
          if (newline) writer += '\n'
          writer ++= valueEscaped
          if (newline) writer += '\n'
          writer ++= tripleQuote
        }
      case value                         =>
        writer ++= renderSafeString(value)
        if (newline) writer += '\n'
        else writer += ' '
    }
  }

  private def unsafeRenderField(definition: FieldDefinition, builder: StringBuilder) = definition match {
    case FieldDefinition(description, name, arguments, tpe, directives) =>
      unsafeRenderDescription(description, builder)
      builder ++= s"  $name"
      unsafeRenderArguments(arguments, builder)
      builder ++= s": $tpe"

      unsafeRenderDirectives(directives, builder)
  }

  private def unsafeRenderArguments(definitions: List[InputValueDefinition], builder: StringBuilder) =
    if (definitions.nonEmpty) {
      builder += '('
      var first = true
      definitions.foreach { definition =>
        if (first) first = false
        else builder += ','
        unsafeRenderDescription(definition.description, builder, newline = false)
        builder ++= definition.name
        builder ++= ": "
        builder ++= innerType(definition.ofType)
        definition.defaultValue.foreach(value => builder ++= s" = ${value.toInputString}")
        unsafeRenderDirectives(definition.directives, builder)
      }
      builder += ')'
    }

  private def renderSafeString(value: String) =
    "\"" + value
      .replace("\\", "\\\\")
      .replace("\b", "\\b")
      .replace("\f", "\\f")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\"", "\\\"") + "\""

}
