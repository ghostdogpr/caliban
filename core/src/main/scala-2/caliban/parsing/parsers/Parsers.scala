package caliban.parsing.parsers

import caliban.InputValue
import caliban.parsing.ParsedDocument
import caliban.parsing.adt.Definition.ExecutableDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import fastparse._

private[caliban] object Parsers extends SelectionParsers {
  def argumentDefinition(implicit ev: P[Any]): P[InputValueDefinition]        =
    P(stringValue.? ~ name ~ ":" ~ type_ ~ defaultValue.? ~ directives.?).map {
      case (description, name, type_, defaultValue, directives) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
    }
  def argumentDefinitions(implicit ev: P[Any]): P[List[InputValueDefinition]] =
    P("(" ~/ argumentDefinition.rep ~ ")").map(_.toList)

  def fieldDefinition(implicit ev: P[Any]): P[FieldDefinition] =
    P(stringValue.? ~ name ~ argumentDefinitions.? ~ ":" ~ type_ ~ directives.?).map {
      case (description, name, args, type_, directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type_, directives.getOrElse(Nil))
    }

  def variableDefinitions(implicit ev: P[Any]): P[List[VariableDefinition]] =
    P("(" ~/ variableDefinition.rep ~ ")").map(_.toList)

  def variableDefinition(implicit ev: P[Any]): P[VariableDefinition] =
    P(variableValue ~ ":" ~/ type_ ~ defaultValue.? ~ directives).map { case (v, t, default, dirs) =>
      VariableDefinition(v.name, t, default, dirs)
    }
  def defaultValue(implicit ev: P[Any]): P[InputValue]               = P("=" ~/ value)

  def operationType(implicit ev: P[Any]): P[OperationType] =
    P("query").map(_ => OperationType.Query) | P("mutation").map(_ => OperationType.Mutation) | P("subscription").map(
      _ => OperationType.Subscription
    )

  def operationDefinition(implicit ev: P[Any]): P[OperationDefinition] =
    P(operationType ~/ name.? ~ variableDefinitions.? ~ directives ~ selectionSet).map {
      case (operationType, name, variableDefinitions, directives, selection) =>
        OperationDefinition(operationType, name, variableDefinitions.getOrElse(Nil), directives, selection)
    } | P(selectionSet).map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection))

  def fragmentDefinition(implicit ev: P[Any]): P[FragmentDefinition] =
    P("fragment" ~/ fragmentName ~ typeCondition ~ directives ~ selectionSet).map {
      case (name, typeCondition, dirs, sel) => FragmentDefinition(name, typeCondition, dirs, sel)
    }

  def objectTypeDefinition(implicit ev: P[Any]): P[ObjectTypeDefinition] =
    P(stringValue.? ~ "type" ~/ name ~ implements.? ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (description, name, implements, directives, fields) =>
        ObjectTypeDefinition(
          description.map(_.value),
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.toList
        )
    }

  def implements(implicit ev: P[Any]): P[List[NamedType]] =
    P("implements" ~ ("&".? ~ namedType) ~ ("&" ~ namedType).rep).map { case (head, tail) =>
      head :: tail.toList
    }

  def interfaceTypeDefinition(implicit ev: P[Any]): P[InterfaceTypeDefinition] =
    P(stringValue.? ~ "interface" ~/ name ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (description, name, directives, fields) =>
        InterfaceTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields.toList)
    }

  def inputObjectTypeDefinition(implicit ev: P[Any]): P[InputObjectTypeDefinition] =
    P(stringValue.? ~ "input" ~/ name ~ directives.? ~ ("{" ~ argumentDefinition.rep ~ "}").?).map {
      case (description, name, directives, fields) =>
        InputObjectTypeDefinition(
          description.map(_.value),
          name,
          directives = directives.getOrElse(Nil),
          fields = fields.fold(List[InputValueDefinition]())(_.toList)
        )
    }

  def enumValueDefinition(implicit ev: P[Any]): P[EnumValueDefinition] =
    P(stringValue.? ~ name ~ directives.?).map { case (description, enumValue, directives) =>
      EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
    }

  def enumName(implicit ev: P[Any]): P[String] = name.filter(s => s != "true" && s != "false" && s != "null")

  def enumTypeDefinition(implicit ev: P[Any]): P[EnumTypeDefinition] =
    P(stringValue.? ~ "enum" ~/ enumName ~ directives.? ~ ("{" ~ enumValueDefinition.rep ~ "}").?).map {
      case (description, name, directives, enumValuesDefinition) =>
        EnumTypeDefinition(
          description.map(_.value),
          name,
          directives = directives.getOrElse(Nil),
          enumValuesDefinition.fold(List[EnumValueDefinition]())(_.toList)
        )
    }

  def unionTypeDefinition(implicit ev: P[Any]): P[UnionTypeDefinition] =
    P(stringValue.? ~ "union" ~/ name ~ directives.? ~ "=" ~ ("|".? ~ namedType) ~ ("|" ~ namedType).rep).map {
      case (description, name, directives, m, ms) =>
        UnionTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  def scalarTypeDefinition(implicit ev: P[Any]): P[ScalarTypeDefinition] =
    P(stringValue.? ~ "scalar" ~/ name ~ directives.?).map { case (description, name, directives) =>
      ScalarTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil))
    }

  def rootOperationTypeDefinition(implicit ev: P[Any]): P[(OperationType, NamedType)] = P(
    operationType ~ ":" ~ namedType
  )

  def schemaDefinition(implicit ev: P[Any]): P[SchemaDefinition] =
    P("schema" ~/ directives.? ~ "{" ~ rootOperationTypeDefinition.rep ~ "}").map { case (directives, ops) =>
      val opsMap = ops.toMap
      SchemaDefinition(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  def schemaExtensionWithOptionalDirectivesAndOperations(implicit ev: P[Any]): P[SchemaExtension] =
    P(directives.? ~ "{" ~ rootOperationTypeDefinition.rep ~ "}").map { case (directives, ops) =>
      val opsMap = ops.toMap
      SchemaExtension(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  def schemaExtensionWithDirectives(implicit ev: P[Any]): P[SchemaExtension] =
    P(directives).map(SchemaExtension(_, None, None, None))

  def schemaExtension(implicit ev: P[Any]): P[SchemaExtension] =
    P("extend schema" ~/ (schemaExtensionWithOptionalDirectivesAndOperations | schemaExtensionWithDirectives))

  def scalarTypeExtension(implicit ev: P[Any]): P[ScalarTypeExtension] =
    P("extend scalar" ~/ name ~ directives).map { case (name, directives) =>
      ScalarTypeExtension(name, directives)
    }

  def objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields(implicit
    ev: P[Any]
  ): P[ObjectTypeExtension] =
    P(name ~ implements.? ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (name, implements, directives, fields) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.toList
        )
    }

  def objectTypeExtensionWithOptionalInterfacesAndDirectives(implicit ev: P[Any]): P[ObjectTypeExtension] =
    P(name ~ implements.? ~ directives ~ !("{" ~ fieldDefinition.rep ~ "}")).map {
      case (name, implements, directives) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives,
          Nil
        )
    }

  def objectTypeExtensionWithInterfaces(implicit ev: P[Any]): P[ObjectTypeExtension] =
    P(name ~ implements).map { case (name, implements) =>
      ObjectTypeExtension(
        name,
        implements,
        Nil,
        Nil
      )
    }

  def objectTypeExtension(implicit ev: P[Any]): P[ObjectTypeExtension] =
    P(
      "extend type" ~/ (
        objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields |
          objectTypeExtensionWithOptionalInterfacesAndDirectives |
          objectTypeExtensionWithInterfaces
      )
    )

  def interfaceTypeExtensionWithOptionalDirectivesAndFields(implicit ev: P[Any]): P[InterfaceTypeExtension] =
    P(name ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map { case (name, directives, fields) =>
      InterfaceTypeExtension(name, directives.getOrElse(Nil), fields.toList)
    }

  def interfaceTypeExtensionWithDirectives(implicit ev: P[Any]): P[InterfaceTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      InterfaceTypeExtension(name, directives, Nil)
    }

  def interfaceTypeExtension(implicit ev: P[Any]): P[InterfaceTypeExtension] =
    P(
      "extend interface" ~/ (
        interfaceTypeExtensionWithOptionalDirectivesAndFields |
          interfaceTypeExtensionWithDirectives
      )
    )

  def unionTypeExtensionWithOptionalDirectivesAndUnionMembers(implicit ev: P[Any]): P[UnionTypeExtension] =
    P(name ~ directives.? ~ "=" ~ ("|".? ~ namedType) ~ ("|" ~ namedType).rep).map { case (name, directives, m, ms) =>
      UnionTypeExtension(name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  def unionTypeExtensionWithDirectives(implicit ev: P[Any]): P[UnionTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      UnionTypeExtension(name, directives, Nil)
    }

  def unionTypeExtension(implicit ev: P[Any]): P[UnionTypeExtension] =
    P("extend union" ~/ (unionTypeExtensionWithOptionalDirectivesAndUnionMembers | unionTypeExtensionWithDirectives))

  def enumTypeExtensionWithOptionalDirectivesAndValues(implicit ev: P[Any]): P[EnumTypeExtension] =
    P(enumName ~ directives.? ~ "{" ~ enumValueDefinition.rep ~ "}").map {
      case (name, directives, enumValuesDefinition) =>
        EnumTypeExtension(name, directives.getOrElse(Nil), enumValuesDefinition.toList)
    }

  def enumTypeExtensionWithDirectives(implicit ev: P[Any]): P[EnumTypeExtension] =
    P(enumName ~ directives).map { case (name, directives) =>
      EnumTypeExtension(name, directives, Nil)
    }

  def enumTypeExtension(implicit ev: P[Any]): P[EnumTypeExtension] =
    P("extend enum" ~/ (enumTypeExtensionWithOptionalDirectivesAndValues | enumTypeExtensionWithDirectives))

  def inputObjectTypeExtensionWithOptionalDirectivesAndFields(implicit ev: P[Any]): P[InputObjectTypeExtension] =
    P(name ~ directives.? ~ "{" ~ argumentDefinition.rep ~ "}").map { case (name, directives, fields) =>
      InputObjectTypeExtension(name, directives.getOrElse(Nil), fields.toList)
    }

  def inputObjectTypeExtensionWithDirectives(implicit ev: P[Any]): P[InputObjectTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      InputObjectTypeExtension(name, directives, Nil)
    }

  def inputObjectTypeExtension(implicit ev: P[Any]): P[InputObjectTypeExtension] =
    P(
      "extend input" ~/ (
        inputObjectTypeExtensionWithOptionalDirectivesAndFields |
          inputObjectTypeExtensionWithDirectives
      )
    )

  def directiveLocation(implicit ev: P[Any]): P[DirectiveLocation] =
    P(
      StringIn(
        "QUERY",
        "MUTATION",
        "SUBSCRIPTION",
        "FIELD",
        "FRAGMENT_DEFINITION",
        "FRAGMENT_SPREAD",
        "INLINE_FRAGMENT",
        "SCHEMA",
        "SCALAR",
        "OBJECT",
        "FIELD_DEFINITION",
        "ARGUMENT_DEFINITION",
        "INTERFACE",
        "UNION",
        "ENUM",
        "ENUM_VALUE",
        "INPUT_OBJECT",
        "INPUT_FIELD_DEFINITION"
      ).!
    ).map {
      case "QUERY"                  => ExecutableDirectiveLocation.QUERY
      case "MUTATION"               => ExecutableDirectiveLocation.MUTATION
      case "SUBSCRIPTION"           => ExecutableDirectiveLocation.SUBSCRIPTION
      case "FIELD"                  => ExecutableDirectiveLocation.FIELD
      case "FRAGMENT_DEFINITION"    => ExecutableDirectiveLocation.FRAGMENT_DEFINITION
      case "FRAGMENT_SPREAD"        => ExecutableDirectiveLocation.FRAGMENT_SPREAD
      case "INLINE_FRAGMENT"        => ExecutableDirectiveLocation.INLINE_FRAGMENT
      case "SCHEMA"                 => TypeSystemDirectiveLocation.SCHEMA
      case "SCALAR"                 => TypeSystemDirectiveLocation.SCALAR
      case "OBJECT"                 => TypeSystemDirectiveLocation.OBJECT
      case "FIELD_DEFINITION"       => TypeSystemDirectiveLocation.FIELD_DEFINITION
      case "ARGUMENT_DEFINITION"    => TypeSystemDirectiveLocation.ARGUMENT_DEFINITION
      case "INTERFACE"              => TypeSystemDirectiveLocation.INTERFACE
      case "UNION"                  => TypeSystemDirectiveLocation.UNION
      case "ENUM"                   => TypeSystemDirectiveLocation.ENUM
      case "ENUM_VALUE"             => TypeSystemDirectiveLocation.ENUM_VALUE
      case "INPUT_OBJECT"           => TypeSystemDirectiveLocation.INPUT_OBJECT
      case "INPUT_FIELD_DEFINITION" => TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION
      case "VARIABLE_DEFINITION"    => TypeSystemDirectiveLocation.VARIABLE_DEFINITION
    }

  def directiveDefinition(implicit ev: P[Any]): P[DirectiveDefinition] =
    P(
      stringValue.? ~ "directive @" ~/ name ~ argumentDefinitions.? ~ "repeatable".!.? ~ "on" ~ ("|".? ~ directiveLocation) ~ ("|" ~ directiveLocation).rep
    ).map { case (description, name, args, repeatable, firstLoc, otherLoc) =>
      DirectiveDefinition(
        description.map(_.value),
        name,
        args.getOrElse(Nil),
        repeatable.isDefined,
        otherLoc.toSet + firstLoc
      )
    }

  def typeDefinition(implicit ev: P[Any]): P[TypeDefinition] =
    objectTypeDefinition |
      interfaceTypeDefinition |
      inputObjectTypeDefinition |
      enumTypeDefinition |
      unionTypeDefinition |
      scalarTypeDefinition

  def typeSystemDefinition(implicit ev: P[Any]): P[TypeSystemDefinition] =
    typeDefinition | schemaDefinition | directiveDefinition

  def executableDefinition(implicit ev: P[Any]): P[ExecutableDefinition] =
    P(operationDefinition | fragmentDefinition)

  def typeExtension(implicit ev: P[Any]): P[TypeExtension] =
    objectTypeExtension |
      interfaceTypeExtension |
      inputObjectTypeExtension |
      enumTypeExtension |
      unionTypeExtension |
      scalarTypeExtension

  def typeSystemExtension(implicit ev: P[Any]): P[TypeSystemExtension] =
    schemaExtension | typeExtension

  def definition(implicit ev: P[Any]): P[Definition] = executableDefinition | typeSystemDefinition | typeSystemExtension

  def document(implicit ev: P[Any]): P[ParsedDocument] =
    P(Start ~ definition.rep ~ End).map(seq => ParsedDocument(seq.toList))
}
