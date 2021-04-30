package caliban.tools

import caliban.parsing.adt._
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._

object RemoteSchema {

  /**
   * Turns an introspection schema into a caliban.parsing.adt.__Schema
   * which can in turn be used for more advanced use cases such as schema
   * stitching.
   */
  def parseRemoteSchema(doc: Document): Option[__Schema] = {
    val queries = doc.schemaDefinition
      .flatMap(_.query)
      .flatMap(doc.objectTypeDefinition(_))

    val mutations = doc.schemaDefinition
      .flatMap(_.mutation)
      .flatMap(doc.objectTypeDefinition(_))

    queries
      .map(queries =>
        __Schema(
          queryType = toObjectType(queries, doc.typeDefinitions),
          mutationType = mutations.map(toObjectType(_, doc.typeDefinitions)),
          subscriptionType = None,
          types = doc.typeDefinitions.map(toTypeDefinition(_, doc.typeDefinitions)),
          directives = doc.directiveDefinitions.map(toDirective(_, doc.typeDefinitions))
        )
      )
  }

  private def toObjectType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.ObjectTypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    __Type(
      kind = __TypeKind.OBJECT,
      name = Some(definition.name),
      description = definition.description,
      interfaces = toInterfaces(definition.implements, definitions),
      directives = toDirectives(definition.directives),
      fields = (args: __DeprecatedArgs) => {
        if (definition.fields.nonEmpty)
          Some(
            definition.fields
              .map(toField(_, definitions))
              .filter(filterDeprecated(_, args))
          )
        else None
      }
    )

  private def toField(
    definition: Definition.TypeSystemDefinition.TypeDefinition.FieldDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Field =
    __Field(
      name = definition.name,
      description = definition.description,
      args = definition.args.map(toInputValue(_, definitions)),
      `type` = toType(definition.ofType, definitions),
      isDeprecated = isDeprecated(definition.directives),
      deprecationReason = deprecationReason(definition.directives),
      directives = toDirectives(definition.directives)
    )

  private def toType(
    definition: Type,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): () => __Type = { () =>
    definition match {
      case Type.ListType(t, nonNull) =>
        if (nonNull)
          __Type(
            kind = __TypeKind.NON_NULL,
            ofType = Some(
              __Type(
                kind = __TypeKind.LIST,
                ofType = Some(
                  toType(t, definitions)()
                )
              )
            )
          )
        else
          __Type(
            kind = __TypeKind.LIST,
            ofType = Some(
              toType(t, definitions)()
            )
          )

      case Type.NamedType(name, nonNull) =>
        if (nonNull)
          __Type(
            kind = __TypeKind.NON_NULL,
            ofType = Some(
              toType(name, definitions)
            )
          )
        else
          toType(name, definitions)
    }
  }

  private def toType(
    name: String,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ) =
    definitions.find(_.name == name) match {
      case Some(value) =>
        value match {
          case e: EnumTypeDefinition        => toEnumType(e)
          case s: ScalarTypeDefinition      => toScalar(s)
          case u: UnionTypeDefinition       => toUnionType(u, definitions)
          case o: ObjectTypeDefinition      => toObjectType(o, definitions)
          case i: InputObjectTypeDefinition =>
            toInputObjectType(i, definitions)
          case i: InterfaceTypeDefinition   =>
            toInterfaceType(i, definitions)
        }
      case None        => __Type(kind = __TypeKind.SCALAR, name = Some(name))
    }

  private def toDirectives(directives: List[Directive]): Option[List[Directive]] = {
    val filtered = directives.filter(_.name != "deprecated")

    if (filtered.nonEmpty) Some(filtered)
    else None
  }

  private def toInterfaces(
    interfaces: List[Type.NamedType],
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): () => Some[List[__Type]] = { () =>
    Some(
      interfaces
        .map(t => toType(t.name, definitions))
    )
  }

  private def toInterfaceType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InterfaceTypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    __Type(
      kind = __TypeKind.INTERFACE,
      name = Some(definition.name),
      description = definition.description,
      fields = (args: __DeprecatedArgs) =>
        if (definition.fields.nonEmpty)
          Some(
            definition.fields
              .map(t => toField(t, definitions))
              .filter(filterDeprecated(_, args))
          )
        else None,
      directives = toDirectives(definition.directives)
    )

  private def toInputValue(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __InputValue =
    __InputValue(
      name = definition.name,
      description = definition.description,
      `type` = toType(definition.ofType, definitions),
      defaultValue = definition.defaultValue.map(_.toString),
      directives = toDirectives(definition.directives)
    )

  private def toEnumType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.EnumTypeDefinition
  ): __Type =
    __Type(
      kind = __TypeKind.ENUM,
      name = Some(definition.name),
      enumValues = (args: __DeprecatedArgs) =>
        if (definition.enumValuesDefinition.nonEmpty)
          Some(definition.enumValuesDefinition.map(toEnumValue(_)).filter(filterDeprecated(_, args)))
        else None,
      directives = toDirectives(definition.directives)
    )

  private def toEnumValue(
    definition: Definition.TypeSystemDefinition.TypeDefinition.EnumValueDefinition
  ): __EnumValue =
    __EnumValue(
      name = definition.enumValue,
      description = definition.description,
      isDeprecated = isDeprecated(definition.directives),
      deprecationReason = deprecationReason(definition.directives)
    )

  private def toInputObjectType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InputObjectTypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    __Type(
      kind = __TypeKind.INPUT_OBJECT,
      name = Some(definition.name),
      description = definition.description,
      inputFields =
        if (definition.fields.nonEmpty)
          Some(
            definition.fields
              .map(f => toInputValue(f, definitions))
          )
        else None,
      directives = toDirectives(definition.directives)
    )

  private def toUnionType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.UnionTypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    __Type(
      kind = __TypeKind.UNION,
      name = Some(definition.name),
      description = definition.description,
      possibleTypes =
        if (definition.memberTypes.nonEmpty)
          Some(
            definition.memberTypes
              .map(t => toType(t, definitions))
          )
        else None,
      directives = toDirectives(definition.directives)
    )

  private def toScalar(
    definition: Definition.TypeSystemDefinition.TypeDefinition.ScalarTypeDefinition
  ): __Type =
    __Type(
      kind = __TypeKind.SCALAR,
      name = Some(definition.name),
      description = definition.description,
      directives = toDirectives(definition.directives)
    )

  private def toTypeDefinition(
    definition: Definition.TypeSystemDefinition.TypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    definition match {
      case o: ObjectTypeDefinition      => toObjectType(o, definitions)
      case s: ScalarTypeDefinition      => toScalar(s)
      case e: EnumTypeDefinition        => toEnumType(e)
      case u: UnionTypeDefinition       => toUnionType(u, definitions)
      case i: InterfaceTypeDefinition   => toInterfaceType(i, definitions)
      case i: InputObjectTypeDefinition =>
        toInputObjectType(i, definitions)
    }

  private def toDirective(
    definition: Definition.TypeSystemDefinition.DirectiveDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Directive =
    __Directive(
      name = definition.name,
      description = definition.description,
      args = definition.args.map(toInputValue(_, definitions)),
      locations = definition.locations.map(toDirectiveLocation(_)).toSet
    )

  private def toDirectiveLocation(loc: DirectiveLocation): __DirectiveLocation =
    loc match {
      case DirectiveLocation.ExecutableDirectiveLocation.QUERY                  => __DirectiveLocation.QUERY
      case DirectiveLocation.ExecutableDirectiveLocation.MUTATION               => __DirectiveLocation.MUTATION
      case DirectiveLocation.ExecutableDirectiveLocation.SUBSCRIPTION           => __DirectiveLocation.SUBSCRIPTION
      case DirectiveLocation.ExecutableDirectiveLocation.FIELD                  => __DirectiveLocation.FIELD
      case DirectiveLocation.ExecutableDirectiveLocation.FRAGMENT_DEFINITION    => __DirectiveLocation.FRAGMENT_DEFINITION
      case DirectiveLocation.ExecutableDirectiveLocation.FRAGMENT_SPREAD        => __DirectiveLocation.FRAGMENT_SPREAD
      case DirectiveLocation.ExecutableDirectiveLocation.INLINE_FRAGMENT        => __DirectiveLocation.INLINE_FRAGMENT
      case DirectiveLocation.TypeSystemDirectiveLocation.SCHEMA                 => __DirectiveLocation.SCHEMA
      case DirectiveLocation.TypeSystemDirectiveLocation.SCALAR                 => __DirectiveLocation.SCALAR
      case DirectiveLocation.TypeSystemDirectiveLocation.OBJECT                 => __DirectiveLocation.OBJECT
      case DirectiveLocation.TypeSystemDirectiveLocation.FIELD_DEFINITION       => __DirectiveLocation.FIELD_DEFINITION
      case DirectiveLocation.TypeSystemDirectiveLocation.ARGUMENT_DEFINITION    => __DirectiveLocation.ARGUMENT_DEFINITION
      case DirectiveLocation.TypeSystemDirectiveLocation.INTERFACE              => __DirectiveLocation.INTERFACE
      case DirectiveLocation.TypeSystemDirectiveLocation.UNION                  => __DirectiveLocation.UNION
      case DirectiveLocation.TypeSystemDirectiveLocation.ENUM                   => __DirectiveLocation.ENUM
      case DirectiveLocation.TypeSystemDirectiveLocation.ENUM_VALUE             => __DirectiveLocation.ENUM_VALUE
      case DirectiveLocation.TypeSystemDirectiveLocation.INPUT_OBJECT           => __DirectiveLocation.INPUT_OBJECT
      case DirectiveLocation.TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION =>
        __DirectiveLocation.INPUT_FIELD_DEFINITION
    }

  private def filterDeprecated(x: __Field, deprecated: __DeprecatedArgs): Boolean =
    if (deprecated.includeDeprecated.getOrElse(true)) true
    else !x.isDeprecated

  private def filterDeprecated(x: __EnumValue, deprecated: __DeprecatedArgs): Boolean =
    if (deprecated.includeDeprecated.getOrElse(true)) true
    else !x.isDeprecated

  private def isDeprecated(directives: List[Directive]): Boolean =
    deprecationReason(directives).isDefined

  private def deprecationReason(directives: List[Directive]): Option[String] =
    directives.collectFirst {
      case d if (d.name == "deprecated") =>
        d.arguments
          .get("reason")
          .collect { case StringValue(value) =>
            value
          }
          .getOrElse("deprecated")
    }
}
