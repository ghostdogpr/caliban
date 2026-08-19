package caliban.tools

import caliban.CalibanError.ValidationError
import caliban.parsing.adt._
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.schema.RootType
import caliban.validation.SchemaValidator

object RemoteSchema {

  private final case class RootNames(query: Option[String], mutation: Option[String], subscription: Option[String])

  /**
   * Turns an introspection schema into a caliban.parsing.adt.__Schema
   * which can in turn be used for more advanced use cases such as schema
   * stitching.
   */
  def parseRemoteSchema(doc: Document): Option[__Schema] = {
    val queries = doc.schemaDefinition
      .flatMap(_.query)
      .flatMap(doc.objectTypeDefinition)

    val mutations = doc.schemaDefinition
      .flatMap(_.mutation)
      .flatMap(doc.objectTypeDefinition)

    val subscriptions = doc.schemaDefinition
      .flatMap(_.subscription)
      .flatMap(doc.objectTypeDefinition)

    queries
      .map(queries =>
        __Schema(
          description = doc.schemaDefinition.flatMap(_.description),
          queryType = toTypeDefinition(queries, doc.typeDefinitions),
          mutationType = mutations.map(toTypeDefinition(_, doc.typeDefinitions)),
          subscriptionType = subscriptions.map(toTypeDefinition(_, doc.typeDefinitions)),
          types = doc.typeDefinitions.map(toTypeDefinition(_, doc.typeDefinitions)),
          directives = doc.directiveDefinitions.map(toDirective(_, doc.typeDefinitions))
        )
      )
  }

  private[caliban] def toRootType(document: Document): Either[ValidationError, RootType] =
    for {
      normalized <- normalizeExtensions(document)
      roots       = rootNames(normalized)
      _          <- SchemaValidator.validateDocument(normalized, roots.query, roots.mutation, roots.subscription)
      rootType    = buildRootType(normalized)
      _          <- SchemaValidator.validateRootType(rootType)
    } yield rootType

  private def normalizeExtensions(document: Document): Either[ValidationError, Document] = {
    val typeExtensions = document.typeExtensions.collect { case extension: TypeExtension => extension }
      .groupBy(extensionName)
    val knownTypes     = document.typeDefinitions.iterator.map(_.name).toSet
    val unknown        = typeExtensions.keys.find(!knownTypes.contains(_))

    unknown match {
      case Some(name) => Left(ValidationError(s"Schema extends undefined type '$name'.", ""))
      case None       =>
        val normalizedTypes =
          document.typeDefinitions.foldLeft[Either[ValidationError, List[TypeDefinition]]](Right(Nil)) {
            case (result, definition) =>
              for {
                definitions <- result
                normalized  <- mergeExtensions(definition, typeExtensions.getOrElse(definition.name, Nil))
              } yield normalized :: definitions
          }

        for {
          types            <- normalizedTypes
          schemaDefinition <- mergeSchemaDeclarations(document)
        } yield {
          val retained = document.definitions.filter {
            case _: TypeDefinition                        => false
            case _: TypeExtension                         => false
            case _: SchemaDefinition | _: SchemaExtension => false
            case _                                        => true
          }
          Document(schemaDefinition.toList ::: types.reverse ::: retained, document.sourceMapper)
        }
    }
  }

  private def mergeSchemaDeclarations(document: Document): Either[ValidationError, Option[SchemaDefinition]] = {
    val definitions = document.definitions.collect { case definition: SchemaDefinition => definition }
    val extensions  = document.typeExtensions.collect { case extension: SchemaExtension => extension }

    if (definitions.size > 1)
      Left(ValidationError("Schema is defined multiple times.", "A schema document may define at most one schema."))
    else if (definitions.isEmpty && extensions.isEmpty) Right(None)
    else
      for {
        query        <- rootDeclaration("query", definitions.map(_.query) ::: extensions.map(_.query))
        mutation     <- rootDeclaration("mutation", definitions.map(_.mutation) ::: extensions.map(_.mutation))
        subscription <- rootDeclaration(
                          "subscription",
                          definitions.map(_.subscription) ::: extensions.map(_.subscription)
                        )
      } yield {
        val conventional                                                     = document.typeDefinitions.iterator.map(_.name).toSet
        def inferred(declared: Option[String], name: String): Option[String] =
          declared.orElse(if (definitions.isEmpty && conventional.contains(name)) Some(name) else None)

        Some(
          SchemaDefinition(
            definitions.flatMap(_.directives) ::: extensions.flatMap(_.directives),
            inferred(query, "Query"),
            inferred(mutation, "Mutation"),
            inferred(subscription, "Subscription"),
            definitions.flatMap(_.description).headOption
          )
        )
      }
  }

  private def rootDeclaration(
    operation: String,
    declarations: List[Option[String]]
  ): Either[ValidationError, Option[String]] =
    declarations.flatten.distinct match {
      case Nil         => Right(None)
      case name :: Nil => Right(Some(name))
      case names       =>
        Left(
          ValidationError(
            s"Conflicting $operation root types are declared: ${names.map(name => s"'$name'").mkString(", ")}.",
            ""
          )
        )
    }

  private def mergeExtensions(
    definition: TypeDefinition,
    extensions: List[TypeExtension]
  ): Either[ValidationError, TypeDefinition] =
    extensions.foldLeft[Either[ValidationError, TypeDefinition]](Right(definition)) { case (result, extension) =>
      result.flatMap {
        case ScalarTypeDefinition(description, name, directives)                        =>
          extension match {
            case ScalarTypeExtension(_, addedDirectives) =>
              Right(ScalarTypeDefinition(description, name, directives ::: addedDirectives))
            case _                                       => extensionKindMismatch(name)
          }
        case ObjectTypeDefinition(description, name, interfaces, directives, fields)    =>
          extension match {
            case ObjectTypeExtension(_, addedInterfaces, addedDirectives, addedFields) =>
              Right(
                ObjectTypeDefinition(
                  description,
                  name,
                  interfaces ::: addedInterfaces,
                  directives ::: addedDirectives,
                  fields ::: addedFields
                )
              )
            case _                                                                     => extensionKindMismatch(name)
          }
        case InterfaceTypeDefinition(description, name, interfaces, directives, fields) =>
          extension match {
            case InterfaceTypeExtension(_, addedDirectives, addedFields) =>
              Right(
                InterfaceTypeDefinition(
                  description,
                  name,
                  interfaces,
                  directives ::: addedDirectives,
                  fields ::: addedFields
                )
              )
            case _                                                       => extensionKindMismatch(name)
          }
        case UnionTypeDefinition(description, name, directives, members)                =>
          extension match {
            case UnionTypeExtension(_, addedDirectives, addedMembers) =>
              Right(UnionTypeDefinition(description, name, directives ::: addedDirectives, members ::: addedMembers))
            case _                                                    => extensionKindMismatch(name)
          }
        case EnumTypeDefinition(description, name, directives, values)                  =>
          extension match {
            case EnumTypeExtension(_, addedDirectives, addedValues) =>
              Right(EnumTypeDefinition(description, name, directives ::: addedDirectives, values ::: addedValues))
            case _                                                  => extensionKindMismatch(name)
          }
        case InputObjectTypeDefinition(description, name, directives, fields)           =>
          extension match {
            case InputObjectTypeExtension(_, addedDirectives, addedFields) =>
              Right(
                InputObjectTypeDefinition(
                  description,
                  name,
                  directives ::: addedDirectives,
                  fields ::: addedFields
                )
              )
            case _                                                         => extensionKindMismatch(name)
          }
      }
    }

  private def extensionKindMismatch(name: String): Either[ValidationError, Nothing] =
    Left(ValidationError(s"Schema extension kind does not match type '$name'.", ""))

  private def extensionName(extension: TypeExtension): String =
    extension match {
      case ScalarTypeExtension(name, _)         => name
      case ObjectTypeExtension(name, _, _, _)   => name
      case InterfaceTypeExtension(name, _, _)   => name
      case UnionTypeExtension(name, _, _)       => name
      case EnumTypeExtension(name, _, _)        => name
      case InputObjectTypeExtension(name, _, _) => name
    }

  private def buildRootType(document: Document): RootType = {
    val roots         = rootNames(document)
    val definitions   = document.typeDefinitions
    val rootTypeNames = roots.query.toSet ++ roots.mutation ++ roots.subscription

    RootType(
      toTypeDefinition(document.objectTypeDefinition(roots.query.get).get, definitions),
      roots.mutation
        .flatMap(document.objectTypeDefinition)
        .map(definition => toTypeDefinition(definition, definitions)),
      roots.subscription
        .flatMap(document.objectTypeDefinition)
        .map(definition => toTypeDefinition(definition, definitions)),
      definitions
        .filterNot(definition => rootTypeNames.contains(definition.name))
        .map(definition => toTypeDefinition(definition, definitions)),
      document.directiveDefinitions.map(definition =>
        withStandardDeprecationDefault(toDirective(definition, definitions))
      ),
      document.schemaDefinition.flatMap(_.description)
    )
  }

  private def withStandardDeprecationDefault(tpe: __Type): __Type = {
    def default(args: __DeprecatedArgs): __DeprecatedArgs =
      if (args.includeDeprecated.isEmpty) args.copy(includeDeprecated = Some(false)) else args

    tpe.copy(
      fields = args => tpe.fields(default(args)),
      enumValues = args => tpe.enumValues(default(args)),
      inputFields = args => tpe.inputFields(default(args))
    )
  }

  private def withTypeMetadata(definition: TypeDefinition, tpe: __Type): __Type = {
    val converted = withStandardDeprecationDefault(tpe)
    definition match {
      case ScalarTypeDefinition(_, _, directives) =>
        converted.copy(specifiedByURL = directives.collectFirst {
          case directive if directive.name == "specifiedBy" =>
            directive.arguments.get("url").collect { case StringValue(value) => value }
        }.flatten)
      case _                                      => converted
    }
  }

  private def withStandardDeprecationDefault(directive: __Directive): __Directive =
    directive.copy(args =
      args =>
        directive.args {
          if (args.includeDeprecated.isEmpty) args.copy(includeDeprecated = Some(false)) else args
        }
    )

  private def rootNames(document: Document): RootNames =
    document.schemaDefinition match {
      case Some(SchemaDefinition(_, query, mutation, subscription, _)) =>
        RootNames(query, mutation, subscription)
      case None                                                        =>
        val names = document.typeDefinitions.iterator.map(_.name).toSet
        RootNames(
          Some("Query"),
          if (names.contains("Mutation")) Some("Mutation") else None,
          if (names.contains("Subscription")) Some("Subscription") else None
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
      fields = (args: __DeprecatedArgs) =>
        if (definition.fields.nonEmpty)
          Some(
            definition.fields
              .map(toField(_, definitions))
              .filter(filterDeprecated(_, args))
          )
        else None
    )

  private def toField(
    definition: Definition.TypeSystemDefinition.TypeDefinition.FieldDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Field =
    __Field(
      name = definition.name,
      description = definition.description,
      args = (args: __DeprecatedArgs) =>
        definition.args
          .map(toInputValue(_, definitions))
          .filter(filterDeprecated(_, args)),
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
      case Some(value) => toTypeDefinition(value, definitions)
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
  ): __Type = {
    val implementations = definitions.collect {
      case t @ ObjectTypeDefinition(_, _, implements, _, _) if implements.map(_.name).toSet.contains(definition.name) =>
        toTypeDefinition(t, definitions)
    }

    __Type(
      kind = __TypeKind.INTERFACE,
      name = Some(definition.name),
      description = definition.description,
      interfaces = toInterfaces(definition.implements, definitions),
      possibleTypes = Some(implementations),
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
  }

  private def toInputValue(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __InputValue =
    __InputValue(
      name = definition.name,
      description = definition.description,
      `type` = toType(definition.ofType, definitions),
      isDeprecated = isDeprecated(definition.directives),
      deprecationReason = deprecationReason(definition.directives),
      defaultValue = definition.defaultValue.map(_.toInputString),
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
          Some(definition.enumValuesDefinition.map(toEnumValue).filter(filterDeprecated(_, args)))
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
      deprecationReason = deprecationReason(definition.directives),
      directives = toDirectives(definition.directives)
    )

  private def toInputObjectType(
    definition: Definition.TypeSystemDefinition.TypeDefinition.InputObjectTypeDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Type =
    __Type(
      kind = __TypeKind.INPUT_OBJECT,
      name = Some(definition.name),
      description = definition.description,
      inputFields = (args: __DeprecatedArgs) =>
        if (definition.fields.nonEmpty)
          Some(
            definition.fields
              .map(toInputValue(_, definitions))
              .filter(filterDeprecated(_, args))
          )
        else None,
      directives = toDirectives(definition.directives),
      isOneOf = Some(Directives.isOneOf(definition.directives))
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
  ): __Type = {
    val converted = definition match {
      case o: ObjectTypeDefinition      => toObjectType(o, definitions)
      case s: ScalarTypeDefinition      => toScalar(s)
      case e: EnumTypeDefinition        => toEnumType(e)
      case u: UnionTypeDefinition       => toUnionType(u, definitions)
      case i: InterfaceTypeDefinition   => toInterfaceType(i, definitions)
      case i: InputObjectTypeDefinition => toInputObjectType(i, definitions)
    }
    withTypeMetadata(definition, converted)
  }

  private def toDirective(
    definition: Definition.TypeSystemDefinition.DirectiveDefinition,
    definitions: List[Definition.TypeSystemDefinition.TypeDefinition]
  ): __Directive =
    __Directive(
      name = definition.name,
      description = definition.description,
      args = (args: __DeprecatedArgs) =>
        definition.args
          .map(toInputValue(_, definitions))
          .filter(filterDeprecated(_, args)),
      isRepeatable = definition.isRepeatable,
      locations = definition.locations.map(toDirectiveLocation)
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
      case DirectiveLocation.TypeSystemDirectiveLocation.VARIABLE_DEFINITION    => __DirectiveLocation.VARIABLE_DEFINITION
    }

  private def filterDeprecated(x: __Field, deprecated: __DeprecatedArgs): Boolean =
    if (deprecated.includeDeprecated.getOrElse(true)) true
    else !x.isDeprecated

  private def filterDeprecated(x: __EnumValue, deprecated: __DeprecatedArgs): Boolean =
    if (deprecated.includeDeprecated.getOrElse(true)) true
    else !x.isDeprecated

  private def filterDeprecated(x: __InputValue, deprecated: __DeprecatedArgs): Boolean =
    if (deprecated.includeDeprecated.getOrElse(true)) true
    else !x.isDeprecated

  private def isDeprecated(directives: List[Directive]): Boolean =
    deprecationReason(directives).isDefined

  private def deprecationReason(directives: List[Directive]): Option[String] =
    directives.collectFirst {
      case d if d.name == "deprecated" =>
        d.arguments
          .get("reason")
          .collect { case StringValue(value) =>
            value
          }
          .getOrElse("No longer supported")
    }
}
