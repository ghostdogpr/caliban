package caliban.rendering

import caliban.InputValue
import caliban.introspection.adt.{ __Type, __TypeKind }
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

  def renderTypeName(t: __Type): String = {
    val builder = new StringBuilder
    __typeNameRenderer.unsafeRender(t, None, builder)
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

  override protected[caliban] def unsafeRender(value: Document, indent: Option[Int], write: StringBuilder): Unit = {
    // Estimate the size of the underlying definitions to prevent re-allocations
    val sizeEstimate = value.sourceMapper.size.getOrElse {
      val numDefs = value.definitions.length
      numDefs * 16 // A naive estimate but a fast one, we just want to get into the ballpark of the actual size
    }
    write.ensureCapacity(sizeEstimate)
    documentRenderer.unsafeRender(value, indent, write)
  }

  private[caliban] lazy val directiveDefinitionsRenderer: Renderer[List[DirectiveDefinition]] =
    directiveDefinitionRenderer.list(Renderer.newline)

  private[caliban] lazy val typesRenderer: Renderer[List[__Type]] =
    typeDefinitionsRenderer.contramap(_.flatMap(_.toTypeDefinition))

  private[caliban] lazy val directivesRenderer: Renderer[List[Directive]] =
    directiveRenderer.list(Renderer.char(' '), omitFirst = false).contramap(_.sortBy(_.name))

  private[caliban] lazy val descriptionRenderer: Renderer[Option[String]] =
    new Renderer[Option[String]] {
      private val tripleQuote = "\"\"\""

      override def unsafeRender(description: Option[String], indent: Option[Int], writer: StringBuilder): Unit =
        description.foreach {
          case value if value.contains('\n') =>
            def valueEscaped(): Unit = unsafeFastEscapeQuote(value, writer)

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
            newlineOrSpace(indent, writer)
          case value                         =>
            pad(indent, writer)
            unsafeFastEscape(value, writer)
            newlineOrSpace(indent, writer)
        }
    }

  private lazy val documentRenderer: Renderer[Document] = Renderer.combine(
    directiveDefinitionsRenderer.contramap(_.directiveDefinitions),
    schemaRenderer.optional.contramap(_.schemaDefinition),
    operationDefinitionRenderer.list(Renderer.newlineOrSpace).contramap(_.operationDefinitions),
    typeDefinitionsRenderer.contramap(_.typeDefinitions),
    fragmentRenderer.list.contramap(_.fragmentDefinitions)
  )

  private lazy val __typeNameRenderer: Renderer[__Type] = new Renderer[__Type] {
    override def unsafeRender(value: __Type, indent: Option[Int], write: StringBuilder): Unit = {
      def loop(typ: Option[__Type]): Unit = typ match {
        case Some(t) =>
          t.kind match {
            case __TypeKind.NON_NULL =>
              loop(t.ofType)
              write append '!'
            case __TypeKind.LIST     =>
              write append '['
              loop(t.ofType)
              write append ']'
            case _                   =>
              write append t.name.getOrElse("null")
          }
        case None    =>
          write append "null"
      }

      loop(Some(value))
    }
  }

  private lazy val directiveDefinitionRenderer: Renderer[DirectiveDefinition] =
    new Renderer[DirectiveDefinition] {
      private val inputRenderer                                       = inputValueDefinitionRenderer.list(Renderer.comma ++ Renderer.spaceOrEmpty)
      private val locationsRenderer: Renderer[Set[DirectiveLocation]] =
        locationRenderer.set(Renderer.spaceOrEmpty ++ Renderer.char('|') ++ Renderer.spaceOrEmpty)

      override def unsafeRender(value: DirectiveDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case DirectiveDefinition(description, name, args, isRepeatable, locations) =>
            descriptionRenderer.unsafeRender(description, indent, write)
            write append "directive @"
            write append name
            if (args.nonEmpty) {
              write append '('
              inputRenderer.unsafeRender(args, indent, write)
              write append ')'
            }
            if (isRepeatable) write append " repeatable"
            write append " on"
            locationsRenderer.unsafeRender(locations, indent, write)
        }

      private[caliban] lazy val locationRenderer: Renderer[DirectiveLocation] =
        new Renderer[DirectiveLocation] {
          override def unsafeRender(location: DirectiveLocation, indent: Option[Int], writer: StringBuilder): Unit =
            location match {
              case ExecutableDirectiveLocation.QUERY                  => writer ++= "QUERY"
              case ExecutableDirectiveLocation.MUTATION               => writer ++= "MUTATION"
              case ExecutableDirectiveLocation.SUBSCRIPTION           => writer ++= "SUBSCRIPTION"
              case ExecutableDirectiveLocation.FIELD                  => writer ++= "FIELD"
              case ExecutableDirectiveLocation.FRAGMENT_DEFINITION    => writer ++= "FRAGMENT_DEFINITION"
              case ExecutableDirectiveLocation.FRAGMENT_SPREAD        => writer ++= "FRAGMENT_SPREAD"
              case ExecutableDirectiveLocation.INLINE_FRAGMENT        => writer ++= "INLINE_FRAGMENT"
              case TypeSystemDirectiveLocation.SCHEMA                 => writer ++= "SCHEMA"
              case TypeSystemDirectiveLocation.SCALAR                 => writer ++= "SCALAR"
              case TypeSystemDirectiveLocation.OBJECT                 => writer ++= "OBJECT"
              case TypeSystemDirectiveLocation.FIELD_DEFINITION       => writer ++= "FIELD_DEFINITION"
              case TypeSystemDirectiveLocation.ARGUMENT_DEFINITION    => writer ++= "ARGUMENT_DEFINITION"
              case TypeSystemDirectiveLocation.INTERFACE              => writer ++= "INTERFACE"
              case TypeSystemDirectiveLocation.UNION                  => writer ++= "UNION"
              case TypeSystemDirectiveLocation.ENUM                   => writer ++= "ENUM"
              case TypeSystemDirectiveLocation.ENUM_VALUE             => writer ++= "ENUM_VALUE"
              case TypeSystemDirectiveLocation.INPUT_OBJECT           => writer ++= "INPUT_OBJECT"
              case TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION => writer ++= "INPUT_FIELD_DEFINITION"
              case TypeSystemDirectiveLocation.VARIABLE_DEFINITION    => writer ++= "VARIABLE_DEFINITION"
            }
        }

    }

  private lazy val operationDefinitionRenderer: Renderer[OperationDefinition] =
    new Renderer[OperationDefinition] {
      override def unsafeRender(definition: OperationDefinition, indent: Option[Int], writer: StringBuilder): Unit =
        definition match {
          case OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet) =>
            operationTypeRenderer.unsafeRender(operationType, indent, writer)
            name.foreach { n =>
              writer append ' '
              writer append n
            }
            variableDefinitionsRenderer.unsafeRender(variableDefinitions, indent, writer)
            directivesRenderer.unsafeRender(directives, indent, writer)
            selectionsRenderer.unsafeRender(selectionSet, indent, writer)
        }
    }

  private lazy val operationTypeRenderer: Renderer[OperationType] = new Renderer[OperationType] {
    override def unsafeRender(operationType: OperationType, indent: Option[Int], write: StringBuilder): Unit =
      operationType match {
        case OperationType.Query        => write append "query"
        case OperationType.Mutation     => write append "mutation"
        case OperationType.Subscription => write append "subscription"
      }
  }

  private lazy val variableDefinitionsRenderer: Renderer[List[VariableDefinition]] =
    new Renderer[List[VariableDefinition]] {
      private val inner = variableDefinition.list(Renderer.comma ++ Renderer.spaceOrEmpty)

      override def unsafeRender(value: List[VariableDefinition], indent: Option[Int], write: StringBuilder): Unit =
        if (value.nonEmpty) {
          write append '('
          inner.unsafeRender(value, indent, write)
          write append ')'
        }
    }

  private lazy val variableDefinition: Renderer[VariableDefinition] = new Renderer[VariableDefinition] {
    override def unsafeRender(definition: VariableDefinition, indent: Option[Int], builder: StringBuilder): Unit = {
      builder append '$'
      builder append definition.name
      builder append ':'
      space(indent, builder)
      typeRenderer.unsafeRender(definition.variableType, indent, builder)
      defaultValueRenderer.unsafeRender(definition.defaultValue, indent, builder)
    }
  }

  private lazy val selectionsRenderer: Renderer[List[Selection]] = new Renderer[List[Selection]] {
    private val inner = selectionRenderer.list(Renderer.newlineOrSpace)

    override def unsafeRender(selections: List[Selection], indent: Option[Int], builder: StringBuilder): Unit = {
      space(indent, builder)
      builder append '{'
      inner.unsafeRender(selections, increment(indent), builder)
      newline(indent, builder)
      pad(indent, builder)
      builder append '}'
    }
  }

  private lazy val selectionRenderer: Renderer[Selection] = new Renderer[Selection] {
    override protected[caliban] def unsafeRender(
      selection: Selection,
      indent: Option[Int],
      builder: StringBuilder
    ): Unit = {
      pad(indent, builder)
      selection match {
        case Selection.Field(alias, name, arguments, directives, selectionSet, _) =>
          alias.foreach { a =>
            builder append a
            builder append ':'
            space(indent, builder)
          }
          builder append name
          inputArgumentsRenderer.unsafeRender(arguments, indent, builder)
          directivesRenderer.unsafeRender(directives, indent, builder)
          if (selectionSet.nonEmpty) {
            selectionsRenderer.unsafeRender(selectionSet, increment(indent), builder)
          }
        case Selection.FragmentSpread(name, directives)                           =>
          builder append "..."
          builder append name
          directivesRenderer.unsafeRender(directives, indent, builder)
        case Selection.InlineFragment(typeCondition, dirs, selectionSet)          =>
          builder append "..."
          typeCondition.foreach { t =>
            space(indent, builder)
            builder append "on "
            builder append t.name
          }
          directivesRenderer.unsafeRender(dirs, indent, builder)
          if (selectionSet.nonEmpty) {
            selectionsRenderer.unsafeRender(selectionSet, increment(indent), builder)
          }
      }
    }
  }

  private lazy val inputArgumentsRenderer: Renderer[Map[String, InputValue]] =
    new Renderer[Map[String, InputValue]] {
      private val inner =
        Renderer.map(
          Renderer.string,
          ValueRenderer.inputValueRenderer,
          Renderer.comma ++ Renderer.spaceOrEmpty,
          Renderer.char(':') ++ Renderer.spaceOrEmpty
        )

      override def unsafeRender(arguments: Map[String, InputValue], indent: Option[Int], builder: StringBuilder): Unit =
        if (arguments.nonEmpty) {
          builder append '('
          inner.unsafeRender(arguments, indent, builder)
          builder append ')'
        }
    }

  private lazy val schemaRenderer: Renderer[SchemaDefinition] = new Renderer[SchemaDefinition] {
    override def unsafeRender(definition: SchemaDefinition, indent: Option[Int], write: StringBuilder): Unit =
      definition match {
        case SchemaDefinition(directives, query, mutation, subscription, description) =>
          val hasTypes    = query.nonEmpty || mutation.nonEmpty || subscription.nonEmpty
          val isExtension = directives.nonEmpty && !hasTypes
          var first       = true

          def renderOp(name: String, op: Option[String]): Unit =
            op.foreach { o =>
              if (first) {
                first = false
                newline(indent, write)
              } else newlineOrComma(indent, write)
              pad(increment(indent), write)
              write append name
              write append ':'
              space(indent, write)
              write append o
            }

          descriptionRenderer.unsafeRender(description, indent, write)
          if (isExtension) write append "extend "
          if (isExtension || hasTypes) {
            write append "schema"
            directivesRenderer.unsafeRender(directives, indent, write)
            if (hasTypes) {
              space(indent, write)
              write append '{'
              renderOp("query", query)
              renderOp("mutation", mutation)
              renderOp("subscription", subscription)
              newline(indent, write)
              write append '}'
            }
          }
      }
  }

  private lazy val typeDefinitionsRenderer: Renderer[List[TypeDefinition]] =
    typeDefinitionRenderer.list.contramap(_.sorted)

  private lazy val typeDefinitionRenderer: Renderer[TypeDefinition] = new Renderer[TypeDefinition] {
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

  private lazy val fragmentRenderer: Renderer[FragmentDefinition] = new Renderer[FragmentDefinition] {
    override def unsafeRender(value: FragmentDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case FragmentDefinition(name, typeCondition, directives, selectionSet) =>
          newlineOrSpace(indent, write)
          write append "fragment "
          write append name
          write append " on "
          write append typeCondition.name
          directivesRenderer.unsafeRender(directives, indent, write)
          selectionsRenderer.unsafeRender(selectionSet, indent, write)
      }
  }

  private lazy val unionRenderer: Renderer[UnionTypeDefinition] = new Renderer[UnionTypeDefinition] {
    private val memberRenderer =
      Renderer.string.list(Renderer.spaceOrEmpty ++ Renderer.char('|') ++ Renderer.spaceOrEmpty)

    override def unsafeRender(value: UnionTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case UnionTypeDefinition(description, name, directives, members) =>
          newlineOrSpace(indent, write)
          descriptionRenderer.unsafeRender(description, indent, write)
          write append "union "
          write append name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write append '='
          space(indent, write)
          memberRenderer.unsafeRender(members, indent, write)
      }
  }

  private lazy val scalarRenderer: Renderer[ScalarTypeDefinition] = new Renderer[ScalarTypeDefinition] {
    override def unsafeRender(value: ScalarTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case ScalarTypeDefinition(description, name, directives) =>
          if (!isBuiltinScalar(name)) {
            newlineOrSpace(indent, write)
            descriptionRenderer.unsafeRender(description, indent, write)
            write append "scalar "
            write append name
            directivesRenderer.unsafeRender(directives, indent, write)
          }
      }
  }

  private lazy val enumRenderer: Renderer[EnumTypeDefinition] = new Renderer[EnumTypeDefinition] {
    private val memberRenderer = enumValueDefinitionRenderer.list(Renderer.newlineOrComma)

    override def unsafeRender(value: EnumTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case EnumTypeDefinition(description, name, directives, values) =>
          newlineOrSpace(indent, write)
          newline(indent, write)
          descriptionRenderer.unsafeRender(description, indent, write)
          write append "enum "
          write append name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write append '{'
          newline(indent, write)
          memberRenderer.unsafeRender(values, increment(indent), write)
          newline(indent, write)
          write append '}'
      }
  }

  private lazy val enumValueDefinitionRenderer: Renderer[EnumValueDefinition] =
    new Renderer[EnumValueDefinition] {
      override def unsafeRender(value: EnumValueDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case EnumValueDefinition(description, name, directives) =>
            descriptionRenderer.unsafeRender(description, indent, write)
            pad(indent, write)
            write append name
            directivesRenderer.unsafeRender(directives, indent, write)
        }
    }

  private lazy val inputObjectTypeDefinition: Renderer[InputObjectTypeDefinition] =
    new Renderer[InputObjectTypeDefinition] {
      private val fieldsRenderer = inputValueDefinitionRenderer.list(Renderer.newlineOrSpace, omitFirst = false)

      override def unsafeRender(value: InputObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InputObjectTypeDefinition(description, name, directives, fields) =>
            newlineOrSpace(indent, write)
            descriptionRenderer.unsafeRender(description, indent, write)
            write append "input "
            write append name
            directivesRenderer.unsafeRender(directives, indent, write)
            space(indent, write)
            write append '{'
            fieldsRenderer.unsafeRender(fields, increment(indent), write)
            newline(indent, write)
            write append '}'
            newline(indent, write)
        }
    }

  private lazy val inputValueDefinitionRenderer: Renderer[InputValueDefinition] =
    new Renderer[InputValueDefinition] {
      override def unsafeRender(
        definition: InputValueDefinition,
        indent: Option[Int],
        builder: StringBuilder
      ): Unit =
        definition match {
          case InputValueDefinition(description, name, valueType, defaultValue, directives) =>
            descriptionRenderer.unsafeRender(description, indent, builder)
            pad(indent, builder)
            builder append name
            builder append ':'
            space(indent, builder)
            typeRenderer.unsafeRender(valueType, indent, builder)
            defaultValueRenderer.unsafeRender(defaultValue, indent, builder)
            directivesRenderer.unsafeRender(directives, indent, builder)
        }
    }

  private lazy val objectTypeDefinitionRenderer: Renderer[ObjectTypeDefinition] =
    new Renderer[ObjectTypeDefinition] {
      override def unsafeRender(value: ObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        unsafeRenderObjectLike(
          "type",
          value.description,
          value.name,
          value.implements,
          value.directives,
          value.fields,
          indent,
          write
        )
    }

  private lazy val interfaceTypeDefinitionRenderer: Renderer[InterfaceTypeDefinition] =
    new Renderer[InterfaceTypeDefinition] {
      override def unsafeRender(value: InterfaceTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        unsafeRenderObjectLike(
          "interface",
          value.description,
          value.name,
          value.implements,
          value.directives,
          value.fields,
          indent,
          write
        )
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
    descriptionRenderer.unsafeRender(description, indent, writer)
    writer append variant
    writer append ' '
    writer append name
    implements match {
      case Nil          =>
      case head :: tail =>
        writer append " implements "
        writer append innerType(head)
        tail.foreach { impl =>
          writer append " & "
          writer append innerType(impl)
        }
    }
    directivesRenderer.unsafeRender(directives, indent, writer)
    if (fields.nonEmpty) {
      space(indent, writer)
      writer append '{'
      fieldDefinitionsRenderer.unsafeRender(fields, increment(indent), writer)
      newline(indent, writer)
      writer append '}'
    }
  }

  private lazy val directiveRenderer: Renderer[Directive] = new Renderer[Directive] {
    override def unsafeRender(d: Directive, indent: Option[Int], writer: StringBuilder): Unit = {
      writer append '@'
      writer append d.name
      inputArgumentsRenderer.unsafeRender(d.arguments, indent, writer)
    }
  }

  private lazy val fieldDefinitionsRenderer: Renderer[List[FieldDefinition]] =
    fieldDefinitionRenderer.list(Renderer.newlineOrSpace, omitFirst = false)

  private lazy val fieldDefinitionRenderer: Renderer[FieldDefinition] = new Renderer[FieldDefinition] {
    override def unsafeRender(definition: FieldDefinition, indent: Option[Int], builder: StringBuilder): Unit =
      definition match {
        case FieldDefinition(description, name, arguments, tpe, directives) =>
          descriptionRenderer.unsafeRender(description, indent, builder)
          pad(indent, builder)
          builder ++= name
          inlineInputValueDefinitionsRenderer.unsafeRender(arguments, indent, builder)
          builder += ':'
          space(indent, builder)
          typeRenderer.unsafeRender(tpe, indent, builder)
          directivesRenderer.unsafeRender(directives, None, builder)
      }
  }

  private lazy val typeRenderer: Renderer[Type] = new Renderer[Type] {
    override def unsafeRender(value: Type, indent: Option[Int], write: StringBuilder): Unit = {
      def loop(t: Type): Unit = t match {
        case Type.NamedType(name, nonNull)  =>
          write append name
          if (nonNull) write append '!'
        case Type.ListType(ofType, nonNull) =>
          write append '['
          loop(ofType)
          write append ']'
          if (nonNull) write append '!'
      }
      loop(value)
    }
  }

  private lazy val inlineInputValueDefinitionsRenderer: Renderer[List[InputValueDefinition]] =
    (Renderer.char('(') ++
      inlineInputValueDefinitionRenderer.list(Renderer.comma ++ Renderer.spaceOrEmpty) ++
      Renderer.char(')')).when(_.nonEmpty)

  private lazy val inlineInputValueDefinitionRenderer: Renderer[InputValueDefinition] =
    new Renderer[InputValueDefinition] {
      override def unsafeRender(definition: InputValueDefinition, indent: Option[Int], builder: StringBuilder): Unit =
        definition match {
          case InputValueDefinition(description, name, tpe, defaultValue, directives) =>
            descriptionRenderer.unsafeRender(description, None, builder)
            builder append name
            builder append ':'
            space(indent, builder)
            typeRenderer.unsafeRender(tpe, indent, builder)
            defaultValueRenderer.unsafeRender(defaultValue, indent, builder)
            directivesRenderer.unsafeRender(directives, None, builder)
        }
    }

  private lazy val defaultValueRenderer: Renderer[Option[InputValue]] = new Renderer[Option[InputValue]] {
    override def unsafeRender(value: Option[InputValue], indent: Option[Int], writer: StringBuilder): Unit =
      value.foreach { value =>
        space(indent, writer)
        writer append '='
        space(indent, writer)
        ValueRenderer.inputValueRenderer.unsafeRender(value, indent, writer)
      }
  }

  private def pad(indentation: Option[Int], writer: StringBuilder): Unit = {
    var i = indentation.getOrElse(0)
    while (i > 0) {
      writer append "  "
      i -= 1
    }
  }

  private[caliban] def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

  private def space(indentation: Option[Int], writer: StringBuilder): Unit =
    if (indentation.isDefined) writer append ' '

  private def newline(indent: Option[Int], writer: StringBuilder): Unit =
    if (indent.isDefined) writer append '\n'

  private def newlineOrSpace(indent: Option[Int], writer: StringBuilder): Unit =
    if (indent.isDefined) writer append '\n' else writer append ' '

  private def newlineOrComma(indentation: Option[Int], writer: StringBuilder): Unit =
    if (indentation.isDefined) writer append '\n' else writer append ','

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
