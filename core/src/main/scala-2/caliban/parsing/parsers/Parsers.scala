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
  def argumentDefinition[_: P]: P[InputValueDefinition]        =
    P(stringValue.? ~ name ~ ":" ~ type_ ~ defaultValue.? ~ directives.?).map {
      case (description, name, type_, defaultValue, directives) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
    }
  def argumentDefinitions[_: P]: P[List[InputValueDefinition]] =
    P("(" ~/ argumentDefinition.rep ~ ")").map(_.toList)

  def fieldDefinition[_: P]: P[FieldDefinition] =
    P(stringValue.? ~ name ~ argumentDefinitions.? ~ ":" ~ type_ ~ directives.?).map {
      case (description, name, args, type_, directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type_, directives.getOrElse(Nil))
    }

  def variableDefinitions[_: P]: P[List[VariableDefinition]] =
    P("(" ~/ variableDefinition.rep ~ ")").map(_.toList)

  def variableDefinition[_: P]: P[VariableDefinition] =
    P(variableValue ~ ":" ~/ type_ ~ defaultValue.? ~ directives).map { case (v, t, default, dirs) =>
      VariableDefinition(v.name, t, default, dirs)
    }
  def defaultValue[_: P]: P[InputValue]               = P("=" ~/ value)

  def operationType[_: P]: P[OperationType] =
    P("query").map(_ => OperationType.Query) | P("mutation").map(_ => OperationType.Mutation) | P("subscription").map(
      _ => OperationType.Subscription
    )

  def operationDefinition[_: P]: P[OperationDefinition] =
    P(operationType ~/ name.? ~ variableDefinitions.? ~ directives ~ selectionSet).map {
      case (operationType, name, variableDefinitions, directives, selection) =>
        OperationDefinition(operationType, name, variableDefinitions.getOrElse(Nil), directives, selection)
    } | P(selectionSet).map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection))

  def fragmentDefinition[_: P]: P[FragmentDefinition] =
    P("fragment" ~/ fragmentName ~ typeCondition ~ directives ~ selectionSet).map {
      case (name, typeCondition, dirs, sel) => FragmentDefinition(name, typeCondition, dirs, sel)
    }

  def objectTypeDefinition[_: P]: P[ObjectTypeDefinition] =
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

  def implements[_: P]: P[List[NamedType]] = P("implements" ~ ("&".? ~ namedType) ~ ("&" ~ namedType).rep).map {
    case (head, tail) => head :: tail.toList
  }

  def interfaceTypeDefinition[_: P]: P[InterfaceTypeDefinition] =
    P(stringValue.? ~ "interface" ~/ name ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (description, name, directives, fields) =>
        InterfaceTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields.toList)
    }

  def inputObjectTypeDefinition[_: P]: P[InputObjectTypeDefinition] =
    P(stringValue.? ~ "input" ~/ name ~ directives.? ~ "{" ~ argumentDefinition.rep ~ "}").map {
      case (description, name, directives, fields) =>
        InputObjectTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields.toList)
    }

  def enumValueDefinition[_: P]: P[EnumValueDefinition] =
    P(stringValue.? ~ name ~ directives.?).map { case (description, enumValue, directives) =>
      EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
    }

  def enumName[_: P]: P[String] = name.filter(s => s != "true" && s != "false" && s != "null")

  def enumTypeDefinition[_: P]: P[EnumTypeDefinition] =
    P(stringValue.? ~ "enum" ~/ enumName ~ directives.? ~ "{" ~ enumValueDefinition.rep ~ "}").map {
      case (description, name, directives, enumValuesDefinition) =>
        EnumTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), enumValuesDefinition.toList)
    }

  def unionTypeDefinition[_: P]: P[UnionTypeDefinition] =
    P(stringValue.? ~ "union" ~/ name ~ directives.? ~ "=" ~ ("|".? ~ namedType) ~ ("|" ~ namedType).rep).map {
      case (description, name, directives, m, ms) =>
        UnionTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  def scalarTypeDefinition[_: P]: P[ScalarTypeDefinition] =
    P(stringValue.? ~ "scalar" ~/ name ~ directives.?).map { case (description, name, directives) =>
      ScalarTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil))
    }

  def rootOperationTypeDefinition[_: P]: P[(OperationType, NamedType)] = P(operationType ~ ":" ~ namedType)

  def schemaDefinition[_: P]: P[SchemaDefinition] =
    P("schema" ~/ directives.? ~ "{" ~ rootOperationTypeDefinition.rep ~ "}").map { case (directives, ops) =>
      val opsMap = ops.toMap
      SchemaDefinition(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  def schemaExtensionWithOptionalDirectivesAndOperations[_: P]: P[SchemaExtension] =
    P(directives.? ~ "{" ~ rootOperationTypeDefinition.rep ~ "}").map { case (directives, ops) =>
      val opsMap = ops.toMap
      SchemaExtension(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  def schemaExtensionWithDirectives[_: P]: P[SchemaExtension] =
    P(directives).map(SchemaExtension(_, None, None, None))

  def schemaExtension[_: P]: P[SchemaExtension] =
    P("extend schema" ~/ (schemaExtensionWithOptionalDirectivesAndOperations | schemaExtensionWithDirectives))

  def scalarTypeExtension[_: P]: P[ScalarTypeExtension] =
    P("extend scalar" ~/ name ~ directives).map { case (name, directives) =>
      ScalarTypeExtension(name, directives)
    }

  def objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields[_: P]: P[ObjectTypeExtension] =
    P(name ~ implements.? ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (name, implements, directives, fields) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.toList
        )
    }

  def objectTypeExtensionWithOptionalInterfacesAndDirectives[_: P]: P[ObjectTypeExtension] =
    P(name ~ implements.? ~ directives ~ !("{" ~ fieldDefinition.rep ~ "}")).map {
      case (name, implements, directives) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives,
          Nil
        )
    }

  def objectTypeExtensionWithInterfaces[_: P]: P[ObjectTypeExtension] =
    P(name ~ implements).map { case (name, implements) =>
      ObjectTypeExtension(
        name,
        implements,
        Nil,
        Nil
      )
    }

  def objectTypeExtension[_: P]: P[ObjectTypeExtension] =
    P(
      "extend type" ~/ (
        objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields |
          objectTypeExtensionWithOptionalInterfacesAndDirectives |
          objectTypeExtensionWithInterfaces
      )
    )

  def interfaceTypeExtensionWithOptionalDirectivesAndFields[_: P]: P[InterfaceTypeExtension] =
    P(name ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map { case (name, directives, fields) =>
      InterfaceTypeExtension(name, directives.getOrElse(Nil), fields.toList)
    }

  def interfaceTypeExtensionWithDirectives[_: P]: P[InterfaceTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      InterfaceTypeExtension(name, directives, Nil)
    }

  def interfaceTypeExtension[_: P]: P[InterfaceTypeExtension] =
    P(
      "extend interface" ~/ (
        interfaceTypeExtensionWithOptionalDirectivesAndFields |
          interfaceTypeExtensionWithDirectives
      )
    )

  def unionTypeExtensionWithOptionalDirectivesAndUnionMembers[_: P]: P[UnionTypeExtension] =
    P(name ~ directives.? ~ "=" ~ ("|".? ~ namedType) ~ ("|" ~ namedType).rep).map { case (name, directives, m, ms) =>
      UnionTypeExtension(name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  def unionTypeExtensionWithDirectives[_: P]: P[UnionTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      UnionTypeExtension(name, directives, Nil)
    }

  def unionTypeExtension[_: P]: P[UnionTypeExtension] =
    P("extend union" ~/ (unionTypeExtensionWithOptionalDirectivesAndUnionMembers | unionTypeExtensionWithDirectives))

  def enumTypeExtensionWithOptionalDirectivesAndValues[_: P]: P[EnumTypeExtension] =
    P(enumName ~ directives.? ~ "{" ~ enumValueDefinition.rep ~ "}").map {
      case (name, directives, enumValuesDefinition) =>
        EnumTypeExtension(name, directives.getOrElse(Nil), enumValuesDefinition.toList)
    }

  def enumTypeExtensionWithDirectives[_: P]: P[EnumTypeExtension] =
    P(enumName ~ directives).map { case (name, directives) =>
      EnumTypeExtension(name, directives, Nil)
    }

  def enumTypeExtension[_: P]: P[EnumTypeExtension] =
    P("extend enum" ~/ (enumTypeExtensionWithOptionalDirectivesAndValues | enumTypeExtensionWithDirectives))

  def inputObjectTypeExtensionWithOptionalDirectivesAndFields[_: P]: P[InputObjectTypeExtension] =
    P(name ~ directives.? ~ "{" ~ argumentDefinition.rep ~ "}").map { case (name, directives, fields) =>
      InputObjectTypeExtension(name, directives.getOrElse(Nil), fields.toList)
    }

  def inputObjectTypeExtensionWithDirectives[_: P]: P[InputObjectTypeExtension] =
    P(name ~ directives).map { case (name, directives) =>
      InputObjectTypeExtension(name, directives, Nil)
    }

  def inputObjectTypeExtension[_: P]: P[InputObjectTypeExtension] =
    P(
      "extend input" ~/ (
        inputObjectTypeExtensionWithOptionalDirectivesAndFields |
          inputObjectTypeExtensionWithDirectives
      )
    )

  def directiveLocation[_: P]: P[DirectiveLocation] =
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
    }

  def directiveDefinition[_: P]: P[DirectiveDefinition] =
    P(
      stringValue.? ~ "directive @" ~/ name ~ argumentDefinitions.? ~ "on" ~ ("|".? ~ directiveLocation) ~ ("|" ~ directiveLocation).rep
    ).map { case (description, name, args, firstLoc, otherLoc) =>
      DirectiveDefinition(description.map(_.value), name, args.getOrElse(Nil), otherLoc.toSet + firstLoc)
    }

  def typeDefinition[_: P]: P[TypeDefinition] =
    objectTypeDefinition |
      interfaceTypeDefinition |
      inputObjectTypeDefinition |
      enumTypeDefinition |
      unionTypeDefinition |
      scalarTypeDefinition

  def typeSystemDefinition[_: P]: P[TypeSystemDefinition] =
    typeDefinition | schemaDefinition | directiveDefinition

  def executableDefinition[_: P]: P[ExecutableDefinition] =
    P(operationDefinition | fragmentDefinition)

  def typeExtension[_: P]: P[TypeExtension] =
    objectTypeExtension |
      interfaceTypeExtension |
      inputObjectTypeExtension |
      enumTypeExtension |
      unionTypeExtension |
      scalarTypeExtension

  def typeSystemExtension[_: P]: P[TypeSystemExtension] =
    schemaExtension | typeExtension

  def definition[_: P]: P[Definition] = executableDefinition | typeSystemDefinition | typeSystemExtension

  def document[_: P]: P[ParsedDocument] =
    P(Start ~ definition.rep ~ End).map(seq => ParsedDocument(seq.toList))
}
