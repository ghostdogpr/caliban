package caliban.rendering

import caliban.{ InputValue, Rendering }
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{
  EnumTypeDefinition,
  EnumValueDefinition,
  FieldDefinition,
  InputObjectTypeDefinition,
  InputValueDefinition,
  InterfaceTypeDefinition,
  ObjectTypeDefinition,
  ScalarTypeDefinition,
  UnionTypeDefinition
}
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Type.{ innerType, NamedType }
import caliban.parsing.adt.{ Definition, Directive, Document, OperationType, Selection, VariableDefinition }

trait Renderer[-A] { self =>

  def contramap[B](f: B => A): Renderer[B] = new Renderer[B] {
    override def unsafeRender(value: B, indent: Option[Int], write: StringBuilder): Unit =
      self.unsafeRender(f(value), indent, write)
  }

  def optional: Renderer[Option[A]] = new Renderer[Option[A]] {
    override def unsafeRender(value: Option[A], indent: Option[Int], write: StringBuilder): Unit =
      value.foreach(self.unsafeRender(_, indent, write))
  }

  def list: Renderer[List[A]] = new Renderer[List[A]] {
    override def unsafeRender(value: List[A], indent: Option[Int], write: StringBuilder): Unit =
      value.foreach(self.unsafeRender(_, indent, write))
  }

  def list(separator: String): Renderer[List[A]] = new Renderer[List[A]] {
    override def unsafeRender(value: List[A], indent: Option[Int], write: StringBuilder): Unit = {
      var first = true
      value.foreach { v =>
        if (first) first = false
        else write.append(separator)
        self.unsafeRender(v, indent, write)
      }
    }
  }

  def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit
}

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
    documentRenderer.unsafeRender(document, None, builder)
    builder.toString()
  }

  def combine[A](renderers: Renderer[A]*): Renderer[A] = new Renderer[A] {
    override def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit =
      renderers.foreach(_.unsafeRender(value, indent, write))
  }

  private[caliban] lazy val documentRenderer: Renderer[Document] = combine(
    schemaRenderer.optional.contramap[Document](_.schemaDefinition),
    operationDefinitionRenderer.list.contramap[Document](_.operationDefinitions),
    typeDefinitionRenderer.list.contramap[Document](_.typeDefinitions.sorted),
    fragmentRenderer.list.contramap[Document](_.fragmentDefinitions)
  )

  private[caliban] lazy val operationDefinitionRenderer: Renderer[OperationDefinition] =
    new Renderer[OperationDefinition] {
      override def unsafeRender(definition: OperationDefinition, indent: Option[Int], writer: StringBuilder): Unit =
        definition match {
          case OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet) =>
            writer += '\n'
            operationTypeRenderer.unsafeRender(operationType, indent, writer)
            name.foreach { n =>
              writer += ' '
              writer ++= n
            }
            variableDefinitionsRenderer.unsafeRender(variableDefinitions, None, writer)
            directivesRenderer.unsafeRender(directives, None, writer)
            selectionRenderer.unsafeRender(selectionSet, Some(0), writer)
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
      builder ++= ": "
      builder ++= definition.variableType.toString
      definition.defaultValue.foreach { value =>
        builder ++= " = "
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
      builder += ' '
      builder += '{'
      selections.foreach(loop(_, increment(indent), builder))
      builder += '\n'
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
          directivesRenderer.unsafeRender(directives, None, builder)
          if (selectionSet.nonEmpty) {
            builder += ' '
            builder += '{'
            selectionSet.foreach(loop(_, increment(indent), builder))
            builder += '\n'
            pad(indent, builder)
            builder += '}'
          }
        case Selection.FragmentSpread(name, directives)                           =>
          builder += '\n'
          pad(indent, builder)
          builder ++= "..."
          builder ++= name
          directivesRenderer.unsafeRender(directives, None, builder)
        case Selection.InlineFragment(typeCondition, dirs, selectionSet)          =>
          builder += '\n'
          pad(indent, builder)
          builder ++= "..."
          typeCondition.foreach { t =>
            builder ++= " on "
            builder ++= t.name
          }
          directivesRenderer.unsafeRender(dirs, None, builder)
          if (selectionSet.nonEmpty) {
            builder += ' '
            builder += '{'
            selectionSet.foreach(loop(_, increment(indent), builder))
            builder += '\n'
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
          else builder ++= ", "
          builder ++= name
          builder ++= ": "
          builder ++= value.toInputString
        }
        builder += ')'
      }
  }

  lazy val schemaRenderer: Renderer[SchemaDefinition] = new Renderer[SchemaDefinition] {
    override def unsafeRender(definition: SchemaDefinition, indent: Option[Int], write: StringBuilder): Unit =
      definition match {
        case SchemaDefinition(directives, query, mutation, subscription, description) =>
          val isExtension = directives.nonEmpty && query.isEmpty && mutation.isEmpty && subscription.isEmpty

          def renderOp(name: String, op: Option[String]): Unit =
            op.foreach { o =>
              write += '\n'
              pad(Some(1), write)
              write ++= name
              write += ':'
              write += ' '
              write ++= o
            }

          unsafeRenderDescription(description, indent, write)
          if (isExtension) write ++= "extend "
          write ++= "schema"
          directivesRenderer.unsafeRender(directives, None, write)
          if (!isExtension) {
            write ++= " {"
            renderOp("query", query)
            renderOp("mutation", mutation)
            renderOp("subscription", subscription)
            write ++= "\n}\n"
          } else {
            write += '\n'
          }
      }
  }

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
          write += '\n'
          write ++= "fragment "
          write ++= name
          write ++= " on "
          write ++= typeCondition.name
          directivesRenderer.unsafeRender(directives, None, write)
          selectionRenderer.unsafeRender(selectionSet, Some(0), write)
      }
  }

  private[caliban] lazy val unionRenderer: Renderer[UnionTypeDefinition] = new Renderer[UnionTypeDefinition] {
    override def unsafeRender(value: UnionTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case UnionTypeDefinition(description, name, directives, members) =>
          write += '\n'
          unsafeRenderDescription(description, None, write)
          write ++= "union "
          write ++= name
          directivesRenderer.unsafeRender(directives, None, write)
          write ++= " = "
          var first = true
          members.foreach { member =>
            if (first) first = false
            else write ++= " | "
            write ++= member
          }
      }
  }

  private[caliban] lazy val scalarRenderer: Renderer[ScalarTypeDefinition] = new Renderer[ScalarTypeDefinition] {
    override def unsafeRender(value: ScalarTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case ScalarTypeDefinition(description, name, directives) =>
          if (!Rendering.isBuiltinScalar(name)) {
            write += '\n'
            unsafeRenderDescription(description, None, write)
            write ++= "scalar "
            write ++= name
            directivesRenderer.unsafeRender(directives, None, write)
          }
      }
  }

  private[caliban] lazy val enumRenderer: Renderer[EnumTypeDefinition] = new Renderer[EnumTypeDefinition] {
    override def unsafeRender(value: EnumTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case EnumTypeDefinition(description, name, directives, values) =>
          write += '\n'
          unsafeRenderDescription(description, None, write)
          write ++= "enum "
          write ++= name
          directivesRenderer.unsafeRender(directives, None, write)
          write += '{'
          values.foreach { definition =>
            unsafeRenderEnumValueDefinition(definition, write)
            write += '\n'
          }
          write += '}'
      }
  }

  private def unsafeRenderEnumValueDefinition(definition: EnumValueDefinition, builder: StringBuilder): Unit =
    definition match {
      case EnumValueDefinition(description, name, directives) =>
        unsafeRenderDescription(description, Some(1), builder)
        builder ++= name
        directivesRenderer.unsafeRender(directives, None, builder)
    }

  private[caliban] lazy val inputObjectTypeDefinition: Renderer[InputObjectTypeDefinition] =
    new Renderer[InputObjectTypeDefinition] {
      override def unsafeRender(value: InputObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InputObjectTypeDefinition(description, name, directives, fields) =>
            write += '\n'
            unsafeRenderDescription(description, None, write)
            write ++= "input "
            write ++= name
            directivesRenderer.unsafeRender(directives, None, write)
            write += ' '
            write += '{'
            fields.foreach { definition =>
              write += '\n'
              inputValueDefinitionRenderer.unsafeRender(definition, Some(1), write)
            }
            write += '\n'
            write += '}'
            write += '\n'
        }
    }

  private lazy val inputValueDefinitionRenderer
    : Renderer[Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition] =
    new Renderer[Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition] {
      override def unsafeRender(
        definition: Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition,
        indent: Option[Int],
        builder: StringBuilder
      ): Unit =
        definition match {
          case InputValueDefinition(description, name, valueType, defaultValue, directives) =>
            unsafeRenderDescription(description, indent, builder)
            pad(indent, builder)
            builder ++= name
            builder ++= ": "
            builder ++= valueType.toString
            defaultValue.foreach { value =>
              builder ++= " = "
              builder ++= value.toInputString
            }
            directivesRenderer.unsafeRender(directives, None, builder)
        }
    }

  private[caliban] lazy val objectTypeDefinitionRenderer: Renderer[ObjectTypeDefinition] =
    new Renderer[ObjectTypeDefinition] {
      override def unsafeRender(value: ObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case ObjectTypeDefinition(description, name, interfaces, directives, fields) =>
            unsafeRenderObjectLike("type", description, name, interfaces, directives, fields, write)
        }
    }

  private[caliban] lazy val interfaceTypeDefinitionRenderer: Renderer[InterfaceTypeDefinition] =
    new Renderer[InterfaceTypeDefinition] {
      override def unsafeRender(value: InterfaceTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InterfaceTypeDefinition(description, name, implements, directives, fields) =>
            unsafeRenderObjectLike("interface", description, name, implements, directives, fields, write)
        }
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
    directivesRenderer.unsafeRender(directives, None, writer)
    writer += ' '
    writer += '{'
    fields.foreach { field =>
      fieldDefinitionRenderer.unsafeRender(field, Some(1), writer)
    }
    writer += '\n'
    writer += '}'
    writer += '\n'
  }

  private lazy val directiveRenderer: Renderer[Directive] = new Renderer[Directive] {
    override def unsafeRender(d: Directive, indent: Option[Int], writer: StringBuilder): Unit = {
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
  }

  private lazy val directivesRenderer: Renderer[List[Directive]] =
    directiveRenderer.list

  private def unsafeRenderDescription(
    description: Option[String],
    indent: Option[Int],
    writer: StringBuilder,
    newline: Boolean = true
  ): Unit = {
    val tripleQuote = "\"\"\""

    def nlOrSp  = if (newline) writer += '\n' else writer += ' '
    def nlOrNot = if (newline) writer += '\n' else writer

    description.foreach {
      case value if value.contains('\n') =>
        val valueEscaped = value.replace(tripleQuote, s"\\$tripleQuote")
        // check if it ends in quote but it is already escaped
        if (value.endsWith("\\\"")) {
          writer ++= tripleQuote
          nlOrNot
          writer ++= valueEscaped
          nlOrNot
          writer ++= tripleQuote
        } else if (value.last == '"') {
          writer ++= tripleQuote
          nlOrNot
          writer ++= valueEscaped
          nlOrSp
          writer ++= tripleQuote
          // check if it ends in quote. We need to break the sequence of 4 or more '"'
        } else {
          // ok. No quotes at the end of value
          writer ++= tripleQuote
          nlOrNot
          writer ++= valueEscaped
          nlOrNot
          writer ++= tripleQuote
        }
      case value                         =>
        if (newline) pad(indent, writer)
        unsafeFastString(value, writer)
        nlOrSp
    }
  }

  private[caliban] lazy val fieldDefinitionRenderer: Renderer[FieldDefinition] = new Renderer[FieldDefinition] {
    override def unsafeRender(definition: FieldDefinition, indent: Option[Int], builder: StringBuilder): Unit =
      definition match {
        case FieldDefinition(description, name, arguments, tpe, directives) =>
          builder += '\n'
          unsafeRenderDescription(description, indent, builder)
          pad(indent, builder)
          builder ++= name
          unsafeRenderArguments(arguments, builder)
          builder ++= s": $tpe"

          directivesRenderer.unsafeRender(directives, None, builder)
      }
  }

  private def unsafeRenderArguments(definitions: List[InputValueDefinition], builder: StringBuilder): Unit =
    if (definitions.nonEmpty) {
      builder += '('
      var first = true
      definitions.foreach { definition =>
        if (first) first = false
        else builder += ','
        unsafeRenderDescription(definition.description, None, builder, newline = false)
        builder ++= definition.name
        builder ++= ": "
        builder ++= innerType(definition.ofType)
        definition.defaultValue.foreach { value =>
          builder ++= " = "
          builder ++= value.toInputString
        }
        directivesRenderer.unsafeRender(definition.directives, None, builder)
      }
      builder += ')'
    }

  private def pad(indentation: Option[Int], writer: StringBuilder): Unit = {
    var i = indentation.getOrElse(0)
    while (i > 0) {
      writer ++= "  "
      i -= 1
    }
  }

  private def increment(indentation: Option[Int]): Option[Int] = indentation.map(_ + 1)

  private def unsafeFastString(value: String, builder: StringBuilder) = {
    builder.append('"')
    var i = 0
    while (i < value.length) {
      value.charAt(i) match {
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

}
