package caliban.tools

import caliban.CalibanError.ValidationError
import caliban.parsing.adt._
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.schema.RootType
import caliban.validation.SchemaValidator
import caliban.validation.ValidationOps.validateAll

object RemoteSchema {

  private final case class RootNames(query: Option[String], mutation: Option[String], subscription: Option[String])
  private[caliban] final case class Normalized(rootType: RootType, document: Document)

  /**
   * Turns an introspection schema into a caliban.parsing.adt.__Schema
   * which can in turn be used for more advanced use cases such as schema
   * stitching.
   */
  def parseRemoteSchema(doc: Document): Option[__Schema] = {
    val converter = new Converter(doc.typeDefinitions, includeDeprecatedByDefault = true)

    def rootType(name: SchemaDefinition => Option[String]): Option[ObjectTypeDefinition] =
      doc.schemaDefinition.flatMap(name).flatMap(doc.objectTypeDefinition)

    rootType(_.query).map(query =>
      __Schema(
        description = doc.schemaDefinition.flatMap(_.description),
        queryType = converter.toTypeDefinition(query),
        mutationType = rootType(_.mutation).map(converter.toTypeDefinition),
        subscriptionType = rootType(_.subscription).map(converter.toTypeDefinition),
        types = doc.typeDefinitions.map(converter.toTypeDefinition),
        directives = doc.directiveDefinitions.map(converter.toDirective)
      )
    )
  }

  private[caliban] def normalize(
    document: Document,
    extensionsCanDefineTypes: Boolean = false
  ): Either[ValidationError, Normalized] =
    for {
      normalized <- normalizeExtensions(document, extensionsCanDefineTypes)
      roots       = rootNames(normalized)
      queryName  <- roots.query.toRight(SchemaValidator.missingQueryRoot)
      _          <- SchemaValidator.validateDocument(normalized, roots.query, roots.mutation, roots.subscription)
      rootType   <- buildRootType(normalized, roots, queryName)
      _          <- SchemaValidator.validateRootType(rootType)
    } yield Normalized(rootType, normalized)

  private def normalizeExtensions(
    document: Document,
    extensionsCanDefineTypes: Boolean
  ): Either[ValidationError, Document] = {
    val defined        = if (extensionsCanDefineTypes) defineMissingTypes(document) else document
    val knownTypes     = defined.typeDefinitions.iterator.map(_.name).toSet
    val typeExtensions = defined.typeExtensions.collect { case extension: TypeExtension => extension }
      .groupBy(extensionName)

    typeExtensions.keys.find(!knownTypes.contains(_)) match {
      case Some(name) => Left(ValidationError(s"Schema extends undefined type '$name'.", ""))
      case None       =>
        for {
          types            <- validateAll(defined.typeDefinitions)(definition =>
                                mergeExtensions(definition, typeExtensions.getOrElse(definition.name, Nil))
                              )
          schemaDefinition <- mergeSchemaDeclarations(defined)
        } yield {
          val retained = defined.definitions.filter {
            case _: TypeDefinition | _: TypeExtension | _: SchemaDefinition | _: SchemaExtension => false
            case _                                                                               => true
          }
          Document(schemaDefinition.toList ::: types ::: retained, defined.sourceMapper)
        }
    }
  }

  private def defineMissingTypes(document: Document): Document = {
    val definedNames     = document.typeDefinitions.iterator.map(_.name).toSet
    val (_, definitions) =
      document.definitions.foldLeft((definedNames, List.empty[Definition])) {
        case ((names, definitions), extension: TypeExtension) if !names.contains(extensionName(extension)) =>
          (names + extensionName(extension), asDefinition(extension) :: definitions)
        case ((names, definitions), definition)                                                            =>
          (names, definition :: definitions)
      }
    Document(definitions.reverse, document.sourceMapper)
  }

  private def asDefinition(extension: TypeExtension): TypeDefinition =
    extension match {
      case ScalarTypeExtension(name, directives)                     => ScalarTypeDefinition(None, name, directives)
      case ObjectTypeExtension(name, interfaces, directives, fields) =>
        ObjectTypeDefinition(None, name, interfaces, directives, fields)
      case InterfaceTypeExtension(name, directives, fields)          =>
        InterfaceTypeDefinition(None, name, Nil, directives, fields)
      case UnionTypeExtension(name, directives, members)             =>
        UnionTypeDefinition(None, name, directives, members)
      case EnumTypeExtension(name, directives, values)               =>
        EnumTypeDefinition(None, name, directives, values)
      case InputObjectTypeExtension(name, directives, fields)        =>
        InputObjectTypeDefinition(None, name, directives, fields)
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
        val declared = RootNames(query, mutation, subscription)
        val roots    = if (definitions.isEmpty) inferConventionalRoots(document, declared) else declared
        Some(
          SchemaDefinition(
            definitions.flatMap(_.directives) ::: extensions.flatMap(_.directives),
            roots.query,
            roots.mutation,
            roots.subscription,
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
    extensions.foldLeft[Either[ValidationError, TypeDefinition]](Right(definition)) { (merged, extension) =>
      merged.flatMap(mergeExtension(_, extension))
    }

  private def mergeExtension(
    definition: TypeDefinition,
    extension: TypeExtension
  ): Either[ValidationError, TypeDefinition] =
    (definition, extension) match {
      case (definition: ScalarTypeDefinition, ScalarTypeExtension(_, directives))                     =>
        Right(definition.copy(directives = definition.directives ::: directives))
      case (definition: ObjectTypeDefinition, ObjectTypeExtension(_, interfaces, directives, fields)) =>
        Right(
          definition.copy(
            implements = definition.implements ::: interfaces,
            directives = definition.directives ::: directives,
            fields = definition.fields ::: fields
          )
        )
      case (definition: InterfaceTypeDefinition, InterfaceTypeExtension(_, directives, fields))       =>
        Right(definition.copy(directives = definition.directives ::: directives, fields = definition.fields ::: fields))
      case (definition: UnionTypeDefinition, UnionTypeExtension(_, directives, members))              =>
        Right(
          definition.copy(
            directives = definition.directives ::: directives,
            memberTypes = definition.memberTypes ::: members
          )
        )
      case (definition: EnumTypeDefinition, EnumTypeExtension(_, directives, values))                 =>
        Right(
          definition.copy(
            directives = definition.directives ::: directives,
            enumValuesDefinition = definition.enumValuesDefinition ::: values
          )
        )
      case (definition: InputObjectTypeDefinition, InputObjectTypeExtension(_, directives, fields))   =>
        Right(definition.copy(directives = definition.directives ::: directives, fields = definition.fields ::: fields))
      case _                                                                                          =>
        Left(ValidationError(s"Schema extension kind does not match type '${definition.name}'.", ""))
    }

  private def extensionName(extension: TypeExtension): String =
    extension match {
      case ScalarTypeExtension(name, _)         => name
      case ObjectTypeExtension(name, _, _, _)   => name
      case InterfaceTypeExtension(name, _, _)   => name
      case UnionTypeExtension(name, _, _)       => name
      case EnumTypeExtension(name, _, _)        => name
      case InputObjectTypeExtension(name, _, _) => name
    }

  private def buildRootType(
    document: Document,
    roots: RootNames,
    queryName: String
  ): Either[ValidationError, RootType] = {
    val definitions   = document.typeDefinitions
    val rootTypeNames = roots.query.toSet ++ roots.mutation ++ roots.subscription
    val converter     = new Converter(definitions, includeDeprecatedByDefault = false)

    def rootDefinition(operation: String, name: String): Either[ValidationError, ObjectTypeDefinition] =
      document
        .objectTypeDefinition(name)
        .toRight(ValidationError(s"The $operation root type '$name' must be an object type.", ""))

    def optionalRoot(operation: String, name: Option[String]): Either[ValidationError, Option[ObjectTypeDefinition]] =
      name match {
        case Some(value) => rootDefinition(operation, value).map(Some(_))
        case None        => Right(None)
      }

    for {
      queryDefinition <- rootDefinition("query", queryName)
      mutation        <- optionalRoot("mutation", roots.mutation)
      subscription    <- optionalRoot("subscription", roots.subscription)
    } yield RootType(
      converter.toTypeDefinition(queryDefinition),
      mutation.map(converter.toTypeDefinition),
      subscription.map(converter.toTypeDefinition),
      definitions
        .filterNot(definition => rootTypeNames.contains(definition.name))
        .map(converter.toTypeDefinition),
      document.directiveDefinitions.map(converter.toDirective),
      document.schemaDefinition.flatMap(_.description)
    )
  }

  private def rootNames(document: Document): RootNames =
    document.schemaDefinition match {
      case Some(SchemaDefinition(_, query, mutation, subscription, _)) => RootNames(query, mutation, subscription)
      case None                                                        =>
        inferConventionalRoots(document, RootNames(Some("Query"), None, None))
    }

  private def inferConventionalRoots(document: Document, declared: RootNames): RootNames = {
    val names = document.typeDefinitions.iterator.map(_.name).toSet

    def conventional(name: String): Option[String] = Some(name).filter(names.contains)

    RootNames(
      declared.query.orElse(conventional("Query")),
      declared.mutation.orElse(conventional("Mutation")),
      declared.subscription.orElse(conventional("Subscription"))
    )
  }

  private final class Converter(definitions: List[TypeDefinition], includeDeprecatedByDefault: Boolean) {

    def toTypeDefinition(definition: TypeDefinition): __Type =
      definition match {
        case o: ObjectTypeDefinition      => toObjectType(o)
        case s: ScalarTypeDefinition      => toScalar(s)
        case e: EnumTypeDefinition        => toEnumType(e)
        case u: UnionTypeDefinition       => toUnionType(u)
        case i: InterfaceTypeDefinition   => toInterfaceType(i)
        case i: InputObjectTypeDefinition => toInputObjectType(i)
      }

    def toDirective(definition: DirectiveDefinition): __Directive =
      __Directive(
        name = definition.name,
        description = definition.description,
        args = (args: __DeprecatedArgs) => filterDeprecated(definition.args.map(toInputValue), args)(_.isDeprecated),
        isRepeatable = definition.isRepeatable,
        locations = definition.locations.map(toDirectiveLocation)
      )

    private def toObjectType(definition: ObjectTypeDefinition): __Type =
      __Type(
        kind = __TypeKind.OBJECT,
        name = Some(definition.name),
        description = definition.description,
        interfaces = toInterfaces(definition.implements),
        directives = toDirectives(definition.directives),
        fields = toFields(definition.fields)
      )

    private def toInterfaceType(definition: InterfaceTypeDefinition): __Type = {
      val implementations = definitions.collect {
        case t @ ObjectTypeDefinition(_, _, implements, _, _) if implements.exists(_.name == definition.name) =>
          toObjectType(t)
      }

      __Type(
        kind = __TypeKind.INTERFACE,
        name = Some(definition.name),
        description = definition.description,
        interfaces = toInterfaces(definition.implements),
        possibleTypes = Some(implementations),
        fields = toFields(definition.fields),
        directives = toDirectives(definition.directives)
      )
    }

    private def toEnumType(definition: EnumTypeDefinition): __Type =
      __Type(
        kind = __TypeKind.ENUM,
        name = Some(definition.name),
        enumValues = (args: __DeprecatedArgs) =>
          if (definition.enumValuesDefinition.nonEmpty)
            Some(filterDeprecated(definition.enumValuesDefinition.map(toEnumValue), args)(_.isDeprecated))
          else None,
        directives = toDirectives(definition.directives)
      )

    private def toInputObjectType(definition: InputObjectTypeDefinition): __Type =
      __Type(
        kind = __TypeKind.INPUT_OBJECT,
        name = Some(definition.name),
        description = definition.description,
        inputFields = (args: __DeprecatedArgs) =>
          if (definition.fields.nonEmpty)
            Some(filterDeprecated(definition.fields.map(toInputValue), args)(_.isDeprecated))
          else None,
        directives = toDirectives(definition.directives),
        isOneOf = Some(Directives.isOneOf(definition.directives))
      )

    private def toUnionType(definition: UnionTypeDefinition): __Type =
      __Type(
        kind = __TypeKind.UNION,
        name = Some(definition.name),
        description = definition.description,
        possibleTypes = if (definition.memberTypes.nonEmpty) Some(definition.memberTypes.map(namedType)) else None,
        directives = toDirectives(definition.directives)
      )

    private def toScalar(definition: ScalarTypeDefinition): __Type =
      __Type(
        kind = __TypeKind.SCALAR,
        name = Some(definition.name),
        description = definition.description,
        directives = toDirectives(definition.directives),
        specifiedByURL = definition.directives
          .find(_.name == "specifiedBy")
          .flatMap(_.arguments.get("url"))
          .collect { case StringValue(url) => url }
      )

    private def toFields(fields: List[FieldDefinition]): __DeprecatedArgs => Option[List[__Field]] =
      (args: __DeprecatedArgs) =>
        if (fields.nonEmpty)
          Some(filterDeprecated(fields.map(toField), args)(_.isDeprecated))
        else None

    private def toField(definition: FieldDefinition): __Field =
      __Field(
        name = definition.name,
        description = definition.description,
        args = (args: __DeprecatedArgs) => filterDeprecated(definition.args.map(toInputValue), args)(_.isDeprecated),
        `type` = toType(definition.ofType),
        isDeprecated = Directives.isDeprecated(definition.directives),
        deprecationReason = deprecationReason(definition.directives),
        directives = toDirectives(definition.directives)
      )

    private def toInputValue(definition: InputValueDefinition): __InputValue =
      __InputValue(
        name = definition.name,
        description = definition.description,
        `type` = toType(definition.ofType),
        isDeprecated = Directives.isDeprecated(definition.directives),
        deprecationReason = deprecationReason(definition.directives),
        defaultValue = definition.defaultValue.map(_.toInputString),
        directives = toDirectives(definition.directives)
      )

    private def toEnumValue(definition: EnumValueDefinition): __EnumValue =
      __EnumValue(
        name = definition.enumValue,
        description = definition.description,
        isDeprecated = Directives.isDeprecated(definition.directives),
        deprecationReason = deprecationReason(definition.directives),
        directives = toDirectives(definition.directives)
      )

    private def toType(tpe: Type): () => __Type =
      () =>
        tpe match {
          case Type.ListType(ofType, nonNull) =>
            withNonNull(__Type(kind = __TypeKind.LIST, ofType = Some(toType(ofType)())), nonNull)
          case Type.NamedType(name, nonNull)  => withNonNull(namedType(name), nonNull)
        }

    private def withNonNull(tpe: __Type, nonNull: Boolean): __Type =
      if (nonNull) __Type(kind = __TypeKind.NON_NULL, ofType = Some(tpe)) else tpe

    private def namedType(name: String): __Type =
      definitions.find(_.name == name) match {
        case Some(definition) => toTypeDefinition(definition)
        case None             => __Type(kind = __TypeKind.SCALAR, name = Some(name))
      }

    private def toInterfaces(interfaces: List[Type.NamedType]): () => Some[List[__Type]] =
      () => Some(interfaces.map(interface => namedType(interface.name)))

    private def toDirectives(directives: List[Directive]): Option[List[Directive]] = {
      val filtered = directives.filter(_.name != Directives.DeprecatedDirective)
      if (filtered.nonEmpty) Some(filtered) else None
    }

    private def toDirectiveLocation(location: DirectiveLocation): __DirectiveLocation =
      location match {
        case ExecutableDirectiveLocation.QUERY                  => __DirectiveLocation.QUERY
        case ExecutableDirectiveLocation.MUTATION               => __DirectiveLocation.MUTATION
        case ExecutableDirectiveLocation.SUBSCRIPTION           => __DirectiveLocation.SUBSCRIPTION
        case ExecutableDirectiveLocation.FIELD                  => __DirectiveLocation.FIELD
        case ExecutableDirectiveLocation.FRAGMENT_DEFINITION    => __DirectiveLocation.FRAGMENT_DEFINITION
        case ExecutableDirectiveLocation.FRAGMENT_SPREAD        => __DirectiveLocation.FRAGMENT_SPREAD
        case ExecutableDirectiveLocation.INLINE_FRAGMENT        => __DirectiveLocation.INLINE_FRAGMENT
        case TypeSystemDirectiveLocation.SCHEMA                 => __DirectiveLocation.SCHEMA
        case TypeSystemDirectiveLocation.SCALAR                 => __DirectiveLocation.SCALAR
        case TypeSystemDirectiveLocation.OBJECT                 => __DirectiveLocation.OBJECT
        case TypeSystemDirectiveLocation.FIELD_DEFINITION       => __DirectiveLocation.FIELD_DEFINITION
        case TypeSystemDirectiveLocation.ARGUMENT_DEFINITION    => __DirectiveLocation.ARGUMENT_DEFINITION
        case TypeSystemDirectiveLocation.INTERFACE              => __DirectiveLocation.INTERFACE
        case TypeSystemDirectiveLocation.UNION                  => __DirectiveLocation.UNION
        case TypeSystemDirectiveLocation.ENUM                   => __DirectiveLocation.ENUM
        case TypeSystemDirectiveLocation.ENUM_VALUE             => __DirectiveLocation.ENUM_VALUE
        case TypeSystemDirectiveLocation.INPUT_OBJECT           => __DirectiveLocation.INPUT_OBJECT
        case TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION => __DirectiveLocation.INPUT_FIELD_DEFINITION
        case TypeSystemDirectiveLocation.VARIABLE_DEFINITION    => __DirectiveLocation.VARIABLE_DEFINITION
      }

    private def filterDeprecated[A](values: List[A], args: __DeprecatedArgs)(isDeprecated: A => Boolean): List[A] =
      if (args.includeDeprecated.getOrElse(includeDeprecatedByDefault)) values else values.filterNot(isDeprecated)

    private def deprecationReason(directives: List[Directive]): Option[String] =
      if (Directives.isDeprecated(directives))
        Directives.deprecationReason(directives).orElse(Some(Directives.DefaultDeprecationReason))
      else None
  }
}
