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

  private[caliban] lazy val documentRenderer: Renderer[Document] = Renderer.combine(
    directiveDefinitionRenderer.list(Renderer.char('\n')).contramap(_.directiveDefinitions),
    schemaRenderer.optional.contramap(_.schemaDefinition),
    operationDefinitionRenderer.list(Renderer.newlineOrSpace).contramap(_.operationDefinitions),
    typeDefinitionsRenderer.contramap(_.typeDefinitions),
    fragmentRenderer.list.contramap(_.fragmentDefinitions)
  )

  private[caliban] lazy val __typeNameRenderer: Renderer[__Type] = new Renderer[__Type] {
    override def unsafeRender(value: __Type, indent: Option[Int], write: StringBuilder): Unit = {
      def loop(typ: Option[__Type]): Unit = typ match {
        case Some(t) =>
          t.kind match {
            case __TypeKind.NON_NULL =>
              loop(t.ofType)
              write += '!'
            case __TypeKind.LIST     =>
              write += '['
              loop(t.ofType)
              write += ']'
            case _                   =>
              write ++= t.name.getOrElse("null")
          }
        case None    =>
          write ++= "null"
      }

      loop(Some(value))
    }
  }

  private lazy val directiveDefinitionRenderer: Renderer[DirectiveDefinition] =
    new Renderer[DirectiveDefinition] {
      private val inputRenderer                                       = inputValueDefinitionRenderer.list(Renderer.char(',') ++ Renderer.space)
      private val locationsRenderer: Renderer[Set[DirectiveLocation]] =
        locationRenderer.set(Renderer.space ++ Renderer.char('|') ++ Renderer.space)

      override def unsafeRender(value: DirectiveDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case DirectiveDefinition(description, name, args, isRepeatable, locations) =>
            descriptionRenderer.unsafeRender(description, indent, write)
            write ++= "directive @"
            write ++= name
            if (args.nonEmpty) {
              write += '('
              inputRenderer.unsafeRender(args, indent, write)
              write += ')'
            }
            if (isRepeatable) write ++= " repeatable"
            write ++= " on"
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

  private[caliban] lazy val typesRenderer: Renderer[List[__Type]] =
    typeDefinitionsRenderer.contramap(_.flatMap(_.toTypeDefinition))

  private[caliban] lazy val operationDefinitionRenderer: Renderer[OperationDefinition] =
    new Renderer[OperationDefinition] {
      override def unsafeRender(definition: OperationDefinition, indent: Option[Int], writer: StringBuilder): Unit =
        definition match {
          case OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet) =>
            operationTypeRenderer.unsafeRender(operationType, indent, writer)
            name.foreach { n =>
              writer += ' '
              writer ++= n
            }
            variableDefinitionsRenderer.unsafeRender(variableDefinitions, indent, writer)
            directivesRenderer.unsafeRender(directives, indent, writer)
            selectionsRenderer.unsafeRender(selectionSet, indent, writer)
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
      defaultValueRenderer.unsafeRender(definition.defaultValue, indent, builder)
    }
  }

  private[caliban] lazy val variableDefinitionsRenderer: Renderer[List[VariableDefinition]] =
    new Renderer[List[VariableDefinition]] {
      private val inner = variableDefinition.list(Renderer.char(',') ++ Renderer.space)

      override def unsafeRender(value: List[VariableDefinition], indent: Option[Int], write: StringBuilder): Unit =
        if (value.nonEmpty) {
          write += '('
          inner.unsafeRender(value, indent, write)
          write += ')'
        }
    }

  private[caliban] lazy val selectionsRenderer: Renderer[List[Selection]] = new Renderer[List[Selection]] {
    private val inner = selectionRenderer.list(Renderer.newlineOrSpace)

    override def unsafeRender(selections: List[Selection], indent: Option[Int], builder: StringBuilder): Unit = {
      space(indent, builder)
      builder += '{'
      inner.unsafeRender(selections, increment(indent), builder)
      newline(indent, builder)
      pad(indent, builder)
      builder += '}'
    }
  }

  private[caliban] lazy val selectionRenderer: Renderer[Selection] = new Renderer[Selection] {
    override protected[caliban] def unsafeRender(
      selection: Selection,
      indent: Option[Int],
      builder: StringBuilder
    ): Unit = {
      pad(indent, builder)
      selection match {
        case Selection.Field(alias, name, arguments, directives, selectionSet, _) =>
          alias.foreach { a =>
            builder ++= a
            builder += ':'
            space(indent, builder)
          }
          builder ++= name
          inputArgumentsRenderer.unsafeRender(arguments, indent, builder)
          directivesRenderer.unsafeRender(directives, indent, builder)
          if (selectionSet.nonEmpty) {
            selectionsRenderer.unsafeRender(selectionSet, increment(indent), builder)
          }
        case Selection.FragmentSpread(name, directives)                           =>
          builder ++= "..."
          builder ++= name
          directivesRenderer.unsafeRender(directives, indent, builder)
        case Selection.InlineFragment(typeCondition, dirs, selectionSet)          =>
          builder ++= "..."
          typeCondition.foreach { t =>
            space(indent, builder)
            builder ++= "on "
            builder ++= t.name
          }
          directivesRenderer.unsafeRender(dirs, indent, builder)
          if (selectionSet.nonEmpty) {
            selectionsRenderer.unsafeRender(selectionSet, increment(indent), builder)
          }
      }
    }
  }

  private[caliban] lazy val inputArgumentsRenderer: Renderer[Map[String, InputValue]] =
    new Renderer[Map[String, InputValue]] {
      private val inner =
        map(
          Renderer.string,
          ValueRenderer.inputValueRenderer,
          Renderer.char(',') ++ Renderer.space
        )

      override def unsafeRender(arguments: Map[String, InputValue], indent: Option[Int], builder: StringBuilder): Unit =
        if (arguments.nonEmpty) {
          builder += '('
          inner.unsafeRender(arguments, indent, builder)
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
                first = false
                newline(indent, write)
              } else newlineOrComma(indent, write)
              pad(increment(indent), write)
              write ++= name
              write += ':'
              space(indent, write)
              write ++= o
            }

          descriptionRenderer.unsafeRender(description, indent, write)
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

  private[caliban] lazy val typeDefinitionsRenderer: Renderer[List[TypeDefinition]] =
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
          selectionsRenderer.unsafeRender(selectionSet, indent, write)
      }
  }

  private[caliban] lazy val unionRenderer: Renderer[UnionTypeDefinition] = new Renderer[UnionTypeDefinition] {
    private val memberRenderer = Renderer.string.list(Renderer.space ++ Renderer.char('|') ++ Renderer.space)

    override def unsafeRender(value: UnionTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case UnionTypeDefinition(description, name, directives, members) =>
          newlineOrSpace(indent, write)
          descriptionRenderer.unsafeRender(description, indent, write)
          write ++= "union "
          write ++= name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write += '='
          space(indent, write)
          memberRenderer.unsafeRender(members, indent, write)
      }
  }

  private[caliban] lazy val scalarRenderer: Renderer[ScalarTypeDefinition] = new Renderer[ScalarTypeDefinition] {
    override def unsafeRender(value: ScalarTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case ScalarTypeDefinition(description, name, directives) =>
          if (!isBuiltinScalar(name)) {
            newlineOrSpace(indent, write)
            descriptionRenderer.unsafeRender(description, indent, write)
            write ++= "scalar "
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
          }
      }
  }

  private[caliban] lazy val enumRenderer: Renderer[EnumTypeDefinition] = new Renderer[EnumTypeDefinition] {
    private val memberRenderer = enumValueDefinitionRenderer.list(Renderer.newlineOrComma)

    override def unsafeRender(value: EnumTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
      value match {
        case EnumTypeDefinition(description, name, directives, values) =>
          newlineOrSpace(indent, write)
          newline(indent, write)
          descriptionRenderer.unsafeRender(description, indent, write)
          write ++= "enum "
          write ++= name
          directivesRenderer.unsafeRender(directives, indent, write)
          space(indent, write)
          write += '{'
          newline(indent, write)
          memberRenderer.unsafeRender(values, increment(indent), write)
          newline(indent, write)
          write += '}'
      }
  }

  private[caliban] lazy val enumValueDefinitionRenderer: Renderer[EnumValueDefinition] =
    new Renderer[EnumValueDefinition] {
      override def unsafeRender(value: EnumValueDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case EnumValueDefinition(description, name, directives) =>
            descriptionRenderer.unsafeRender(description, indent, write)
            pad(indent, write)
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
        }
    }

  private[caliban] lazy val inputObjectTypeDefinition: Renderer[InputObjectTypeDefinition] =
    new Renderer[InputObjectTypeDefinition] {
      private val fieldsRenderer = inputValueDefinitionRenderer.list(Renderer.newlineOrSpace)

      override def unsafeRender(value: InputObjectTypeDefinition, indent: Option[Int], write: StringBuilder): Unit =
        value match {
          case InputObjectTypeDefinition(description, name, directives, fields) =>
            newlineOrSpace(indent, write)
            descriptionRenderer.unsafeRender(description, indent, write)
            write ++= "input "
            write ++= name
            directivesRenderer.unsafeRender(directives, indent, write)
            space(indent, write)
            write += '{'
            fieldsRenderer.unsafeRender(fields, increment(indent), write)
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
            descriptionRenderer.unsafeRender(description, indent, builder)
            pad(indent, builder)
            builder ++= name
            builder += ':'
            space(indent, builder)
            typeRenderer.unsafeRender(valueType, indent, builder)
            defaultValueRenderer.unsafeRender(defaultValue, indent, builder)
            directivesRenderer.unsafeRender(directives, indent, builder)
        }
    }

  private[caliban] lazy val objectTypeDefinitionRenderer: Renderer[ObjectTypeDefinition] =
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

  private[caliban] lazy val interfaceTypeDefinitionRenderer: Renderer[InterfaceTypeDefinition] =
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
      fieldDefinitionsRenderer.unsafeRender(fields, increment(indent), writer)
      newline(indent, writer)
      writer += '}'
    }
  }

  private lazy val directiveRenderer: Renderer[Directive] = new Renderer[Directive] {
    override def unsafeRender(d: Directive, indent: Option[Int], writer: StringBuilder): Unit = {
      writer += '@'
      writer ++= d.name
      inputArgumentsRenderer.unsafeRender(d.arguments, indent, writer)
    }
  }

  private[caliban] lazy val fieldDefinitionsRenderer: Renderer[List[FieldDefinition]] =
    fieldDefinitionRenderer.list(Renderer.newlineOrSpace, omitFirst = false)

  private[caliban] lazy val fieldDefinitionRenderer: Renderer[FieldDefinition] = new Renderer[FieldDefinition] {
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

  private[caliban] lazy val typeRenderer: Renderer[Type] = new Renderer[Type] {
    override def unsafeRender(value: Type, indent: Option[Int], write: StringBuilder): Unit = {
      def loop(t: Type): Unit = t match {
        case Type.NamedType(name, nonNull)  =>
          write ++= name
          if (nonNull) write += '!'
        case Type.ListType(ofType, nonNull) =>
          write += '['
          loop(ofType)
          write += ']'
          if (nonNull) write += '!'
      }
      loop(value)
    }
  }

  private[caliban] lazy val inlineInputValueDefinitionsRenderer: Renderer[List[InputValueDefinition]] =
    (Renderer.char('(') ++
      inlineInputValueDefinitionRenderer.list(Renderer.char(',') ++ Renderer.space) ++
      Renderer.char(')')).when(_.nonEmpty)

  private[caliban] lazy val inlineInputValueDefinitionRenderer: Renderer[InputValueDefinition] =
    new Renderer[InputValueDefinition] {
      override def unsafeRender(definition: InputValueDefinition, indent: Option[Int], builder: StringBuilder): Unit =
        definition match {
          case InputValueDefinition(description, name, tpe, defaultValue, directives) =>
            descriptionRenderer.unsafeRender(description, None, builder)
            builder ++= name
            builder += ':'
            space(indent, builder)
            typeRenderer.unsafeRender(tpe, indent, builder)
            defaultValueRenderer.unsafeRender(defaultValue, indent, builder)
            directivesRenderer.unsafeRender(directives, None, builder)
        }
    }

  private[caliban] lazy val defaultValueRenderer: Renderer[Option[InputValue]] = new Renderer[Option[InputValue]] {
    override def unsafeRender(value: Option[InputValue], indent: Option[Int], writer: StringBuilder): Unit =
      value.foreach { value =>
        space(indent, writer)
        writer += '='
        space(indent, writer)
        ValueRenderer.inputValueRenderer.unsafeRender(value, indent, writer)
      }
  }

  private def pad(indentation: Option[Int], writer: StringBuilder): Unit = {
    var i = indentation.getOrElse(0)
    while (i > 0) {
      writer ++= "  "
      i -= 1
    }
  }

  def map[K, V](
    keyRender: Renderer[K],
    valueRender: Renderer[V],
    separator: Renderer[Any]
  ): Renderer[Map[K, V]] = new Renderer[Map[K, V]] {
    override def unsafeRender(value: Map[K, V], indent: Option[Int], write: StringBuilder): Unit = {
      var first = true
      value.foreach { case (k, v) =>
        if (first) first = false
        else separator.unsafeRender((), indent, write)
        keyRender.unsafeRender(k, indent, write)
        write.append(':')
        space(indent, write)
        valueRender.unsafeRender(v, indent, write)
      }
    }
  }

  private[caliban] def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

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
