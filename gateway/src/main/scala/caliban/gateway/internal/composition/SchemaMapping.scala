package caliban.gateway.internal.composition

import caliban.execution.Field
import caliban.gateway._
import caliban.gateway.SchemaTransformation._
import caliban.InputValue
import caliban.gateway.internal.execution.ResponseProjection
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.gateway.internal.planning.OperationPlan.RequiredSelection
import caliban.gateway.internal.composition.ComposedGraph.{ ContextName, FieldArgument, TypeField }
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.adt.{ Definition, Directive, Document, Selection, Type }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.Parser
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.Value.{ EnumValue, NullValue, StringValue }

import scala.collection.compat._

private[gateway] final class SchemaMapping private (
  private val sourceRootType: RootType,
  mappings: SchemaMapping.Mappings
) {
  import SchemaMapping._

  val hiddenTypes: Set[String]            = mappings.hiddenTypes
  val hiddenFields: Set[TypeField]        = mappings.hiddenFields
  val hiddenArguments: Set[FieldArgument] = mappings.hiddenArguments
  val hiddenInputFields: Set[TypeField]   = mappings.hiddenInputFields
  lazy val rootNames: OperationRootNames  = sourceRootNames.mapSource(clientType)
  val nonEmpty: Boolean                   = mappings.nonEmpty

  def clientType(name: String): String =
    typeNames.getOrElse(name, name)

  def composedType(name: String): String =
    sourceRootNames.composed(name) match {
      case `name`   => clientType(name)
      case composed => composed
    }

  def sourceType(name: String): String =
    sourceTypes.getOrElse(name, name)

  def transform(document: Document): Document =
    if (nonEmpty) {
      val names            = SchemaComposer.federationDirectiveNames(document)
      val contexts         = document.typeDefinitions.flatMap { tpe =>
        tpe.directives.filter(directive => names.context.contains(directive.name)).flatMap { directive =>
          directive.arguments.get("name").collect { case StringValue(name) => ContextName(name) }.toList.flatMap {
            name =>
              sourceRootType.types.get(tpe.name).toList.flatMap(_.possibleTypeNames.toList.sorted).map(name -> _)
          }
        }
      }.groupMap(_._1)(_._2).map { case (name, declarations) => name -> declarations.distinct }
      val directiveContext = DirectiveContext(names, contexts)
      Document(document.definitions.map(transformDefinition(_, directiveContext)), document.sourceMapper)
    } else document

  def transform(lookup: Lookup): Lookup =
    if (nonEmpty) transformLookup(lookup) else lookup

  def fieldToSource(field: Field): Field = {
    val clientParent       = innerParentTypeName(field)
    val sourceParent       = sourceRootNames.source(clientParent).getOrElse(sourceType(clientParent))
    val sourceName         = sourceField(clientParent, field.name)
    val composedDirectives = field.parentType
      .flatMap(parent => Option(parent.innerType.getFieldOrNull(field.name)))
      .flatMap(_.directives)
      .getOrElse(Nil)
    val arguments          = field.arguments.map { case (name, value) =>
      sourceArguments.getOrElse(FieldArgument(clientParent, field.name, name), name) -> value
    }
    val alias              =
      if (field.alias.isEmpty && sourceName != field.name) Some(field.name)
      else field.alias

    field.copy(
      name = sourceName,
      alias = alias,
      parentType = sourceRootType.types.get(sourceParent),
      fields = field.fields.map(fieldToSource),
      targets = field.targets.map(_.map(sourceType)),
      arguments = arguments,
      directives =
        if (composedDirectives.isEmpty) field.directives
        else field.directives.filterNot(composedDirectives.contains)
    )
  }

  def lookupFieldToSource(field: String): String =
    sourceField(sourceQueryName, field)

  def lookupArgumentsToSource(field: String, arguments: Map[String, InputValue]): Map[String, InputValue] =
    arguments.map { case (name, value) =>
      sourceArguments.getOrElse(FieldArgument(sourceQueryName, field, name), name) -> value
    }

  def representationToSource(typeName: String, value: InputObjectValue): InputObjectValue =
    if (renamesNothing) value
    else mapRepresentation(typeName, value)

  private[internal] def requiredSelectionToSource(
    parentType: String,
    selection: RequiredSelection
  ): RequiredSelection = {
    val sourceParent = sourceType(parentType)
    val sourceName   = sourceField(parentType, selection.field)
    val childType    = sourceFieldDefinition(sourceParent, sourceName).flatMap(_._type.innerType.name).getOrElse("")
    RequiredSelection(
      sourceName,
      selection.responseName,
      selection.children.map(requiredSelectionToSource(clientType(childType), _))
    )
  }

  private[internal] def responseProjection(
    client: List[Field],
    executable: List[Field],
    required: List[RequiredSelection] = Nil
  ): ResponseProjection =
    ResponseProjection.compile(client, executable, required, typeNames)

  private val typeNames      = mappings.typeNames
  private val fieldNames     = mappings.fieldNames
  private val argumentNames  = mappings.argumentNames
  private val renamesNothing = mappings.renamesNothing

  private val sourceRootNames = OperationRootNames(sourceRootType)
  private val sourceQueryName = sourceRootNames.source("Query").getOrElse("Query")

  private def clientOwners(sourceType: String): List[String] =
    clientType(sourceType) :: sourceRootNames.composedAll(sourceType)

  private val sourceTypes     = typeNames.map(_.swap)
  private val sourceFields    = fieldNames.iterator.flatMap { case (TypeField(tpe, field), renamed) =>
    clientOwners(tpe).map(owner => TypeField(owner, renamed) -> field)
  }.toMap
  private val sourceArguments = argumentNames.iterator.flatMap { case (FieldArgument(tpe, field, argument), renamed) =>
    clientOwners(tpe).map(owner => FieldArgument(owner, clientField(tpe, field), renamed) -> argument)
  }.toMap

  private def clientField(typeName: String, field: String): String =
    fieldNames.getOrElse(TypeField(typeName, field), field)

  private def sourceField(typeName: String, field: String): String =
    sourceFields.getOrElse(TypeField(typeName, field), field)

  private def clientArgument(typeName: String, field: String, argument: String): String =
    argumentNames.getOrElse(FieldArgument(typeName, field, argument), argument)

  private def transformLookup(lookup: Lookup): Lookup = {
    def argument(value: Lookup.Argument): Lookup.Argument = value match {
      case Lookup.Argument.Key(field)            => Lookup.Argument.key(clientField(lookup.typeName, field))
      case Lookup.Argument.ObjectMapping(fields) =>
        Lookup.Argument.obj(fields.map { case (name, nested) => name -> argument(nested) }: _*)
      case Lookup.Argument.Batch(value)          => Lookup.Argument.batch(argument(value))
    }
    val arguments                                         = lookup.arguments.map { case (name, value) =>
      clientArgument(sourceQueryName, lookup.field, name) -> argument(value)
    }
    val typeName                                          = clientType(lookup.typeName)
    val field                                             = clientField(sourceQueryName, lookup.field)
    val keys                                              = lookup.keyFields.map(clientField(lookup.typeName, _))

    lookup match {
      case _: Lookup.Single    => Lookup.single(typeName, keys, field, arguments: _*)
      case value: Lookup.ByKey =>
        val correlation = value.correlation.iterator.map { case (response, key) =>
          clientField(lookup.typeName, response) -> clientField(lookup.typeName, key)
        }.toMap
        Lookup.list(typeName, keys, field, correlation, arguments: _*)
    }
  }

  private def mapRepresentation(typeName: String, value: InputObjectValue): InputObjectValue =
    InputObjectValue(value.fields.map {
      case (TypenameField, StringValue(name)) => TypenameField -> StringValue(sourceType(name))
      case (name, nested)                     =>
        val fieldName                                             = sourceField(typeName, name)
        def translate(tpe: __Type, input: InputValue): InputValue = tpe.kind match {
          case __TypeKind.NON_NULL                      => tpe.ofType.fold(input)(translate(_, input))
          case __TypeKind.LIST                          =>
            input match {
              case InputListValue(values) =>
                InputListValue(values.map(value => tpe.ofType.fold(value)(translate(_, value))))
              case other                  => other
            }
          case __TypeKind.OBJECT | __TypeKind.INTERFACE =>
            input match {
              case obj: InputObjectValue => mapRepresentation(clientType(tpe.name.getOrElse("")), obj)
              case other                 => other
            }
          case _                                        => input
        }
        fieldName -> sourceFieldDefinition(sourceType(typeName), fieldName).fold(nested)(field =>
          translate(field._type, nested)
        )
    })

  private def transformDefinition(definition: Definition, directiveContext: DirectiveContext): Definition =
    definition match {
      case value: SchemaDefinition          =>
        value.copy(
          directives = transformDirectives(value.directives, Nil, directiveContext),
          query = value.query.map(clientType),
          mutation = value.mutation.map(clientType),
          subscription = value.subscription.map(clientType)
        )
      case value: DirectiveDefinition       =>
        value.copy(args = value.args.map(transformInputValueDefinition(_, directiveContext)))
      case value: ObjectTypeDefinition      =>
        value.copy(
          name = clientType(value.name),
          implements = value.implements.map(transformNamedType),
          directives = transformDirectives(value.directives, value.name :: Nil, directiveContext),
          fields = value.fields.map(transformFieldDefinition(value.name, _, directiveContext))
        )
      case value: InterfaceTypeDefinition   =>
        value.copy(
          name = clientType(value.name),
          implements = value.implements.map(transformNamedType),
          directives = transformDirectives(value.directives, value.name :: Nil, directiveContext),
          fields = value.fields.map(transformFieldDefinition(value.name, _, directiveContext))
        )
      case value: InputObjectTypeDefinition =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, directiveContext),
          fields = value.fields.map(transformInputValueDefinition(_, directiveContext))
        )
      case value: EnumTypeDefinition        =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, directiveContext),
          enumValuesDefinition = value.enumValuesDefinition.map(enumValue =>
            enumValue.copy(directives = transformDirectives(enumValue.directives, Nil, directiveContext))
          )
        )
      case value: UnionTypeDefinition       =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, directiveContext),
          memberTypes = value.memberTypes.map(clientType)
        )
      case value: ScalarTypeDefinition      =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, directiveContext)
        )
      case other                            => other
    }

  private def transformFieldDefinition(
    typeName: String,
    field: FieldDefinition,
    directiveContext: DirectiveContext
  ): FieldDefinition = {
    val outputType = Type.innerType(field.ofType)
    field.copy(
      name = clientField(typeName, field.name),
      args = field.args.map { argument =>
        transformInputValueDefinition(argument, directiveContext)
          .copy(name = clientArgument(typeName, field.name, argument.name))
      },
      ofType = transformType(field.ofType),
      directives =
        transformDirectives(field.directives, List(typeName, outputType), directiveContext).map { directive =>
          if (directiveContext.names.listSize.contains(directive.name))
            transformListSize(directive, typeName, field.name, outputType)
          else directive
        }
    )
  }

  private def transformListSize(directive: Directive, parent: String, field: String, outputType: String): Directive = {
    def mapStrings(value: InputValue)(rewrite: String => String): InputValue = value match {
      case StringValue(value)     => StringValue(rewrite(value))
      case InputListValue(values) => InputListValue(values.map(value => mapStrings(value)(rewrite)))
      case other                  => other
    }
    directive.copy(arguments = directive.arguments.map {
      case ("slicingArguments", value) =>
        "slicingArguments" -> mapStrings(value) { path =>
          val segments = path.split("\\.", -1).toList
          (clientArgument(parent, field, segments.head) :: segments.tail).mkString(".")
        }
      case ("sizedFields", value)      =>
        "sizedFields" -> mapStrings(value) { selection =>
          parseFieldSet(selection)
            .fold(selection)(fields => renderFieldSet(fields.map(transformFieldSetSelection(outputType, _))))
        }
      case other                       => other
    })
  }

  private def transformFromContext(directive: Directive, directiveContext: DirectiveContext): Directive =
    directive.arguments
      .get("field")
      .collect { case StringValue(value) => value }
      .flatMap(SchemaComposer.parseContextSelection)
      .fold(directive) { case (name, selections) =>
        val rewritten = directiveContext.contextTypes.getOrElse(name, Nil).map { parent =>
          parent -> selections.map(transformFieldSetSelection(parent, _))
        }
        val distinct  = rewritten.map(_._2).distinct
        val fields    =
          if (distinct.size <= 1) distinct.headOption.getOrElse(selections)
          else {
            val fragments = rewritten.head._2.collect { case fragment: Selection.InlineFragment => fragment }
            rewritten.map { case (parent, fields) =>
              Selection.InlineFragment(
                Some(NamedType(clientType(parent), false)),
                Nil,
                fields.filterNot(_.isInstanceOf[Selection.InlineFragment])
              )
            } ::: fragments
          }
        if (fields == selections) directive
        else
          directive.copy(arguments =
            directive.arguments.updated("field", StringValue(s"$$${name.value} { ${renderFieldSet(fields)} }"))
          )
      }

  private def transformDirectives(
    directives: List[Directive],
    candidateTypes: List[String],
    directiveContext: DirectiveContext
  ): List[Directive] =
    directives.map { directive =>
      val names = directiveContext.names
      if (names.fromContext.contains(directive.name)) transformFromContext(directive, directiveContext)
      else if (names.fieldSetDirectives.contains(directive.name))
        transformFieldSet(directive, candidateTypes, names.provides)
      else directive
    }

  private def transformFieldSet(
    directive: Directive,
    candidateTypes: List[String],
    provides: Set[String]
  ): Directive = {
    val transformed = for {
      value      <- directive.arguments.get("fields").collect { case StringValue(value) => value }
      selections <- parseFieldSet(value)
      startType  <- fieldSetParent(directive.name, candidateTypes, selections, provides)
    } yield renderFieldSet(selections.map(transformFieldSetSelection(startType, _)))
    transformed.fold(directive)(fields =>
      directive.copy(arguments = directive.arguments.updated("fields", StringValue(fields)))
    )
  }

  private def fieldSetParent(
    directive: String,
    candidates: List[String],
    selections: List[Selection],
    provides: Set[String]
  ): Option[String] = {
    // @provides selects on the return type; @key and @requires select on the parent type.
    val ordered =
      if (provides.contains(directive)) candidates.reverse
      else candidates
    ordered.find { candidate =>
      val parent = sourceRootType.types.get(candidate)
      selections.forall {
        case field: Selection.Field => parent.exists(tpe => tpe.getFieldOrNull(field.name) ne null)
        case _                      => true
      }
    }
  }

  private def transformFieldSetSelection(parentType: String, selection: Selection): Selection =
    selection match {
      case field: Selection.Field             =>
        val definition = sourceFieldDefinition(parentType, field.name)
        val arguments  = field.arguments.map { case (name, value) =>
          clientArgument(parentType, field.name, name) -> value
        }
        val childType  = definition.flatMap(_._type.innerType.name).getOrElse("")
        field.copy(
          name = clientField(parentType, field.name),
          arguments = arguments,
          selectionSet = field.selectionSet.map(transformFieldSetSelection(childType, _))
        )
      case fragment: Selection.InlineFragment =>
        val nextType = fragment.typeCondition.map(_.name).getOrElse(parentType)
        fragment.copy(
          typeCondition = fragment.typeCondition.map(transformNamedType),
          selectionSet = fragment.selectionSet.map(transformFieldSetSelection(nextType, _))
        )
      case other                              => other
    }

  private def renderFieldSet(selections: List[Selection]): String =
    DocumentRenderer.selectionsRenderer.renderCompact(selections).stripPrefix("{").stripSuffix("}")

  private def transformInputValueDefinition(
    value: InputValueDefinition,
    directiveContext: DirectiveContext
  ): InputValueDefinition =
    value.copy(
      ofType = transformType(value.ofType),
      directives = transformDirectives(value.directives, Nil, directiveContext)
    )

  private def transformType(tpe: Type): Type =
    tpe match {
      case NamedType(name, nonNull)  => NamedType(clientType(name), nonNull)
      case ListType(ofType, nonNull) => ListType(transformType(ofType), nonNull)
    }

  private def transformNamedType(tpe: NamedType): NamedType =
    tpe.copy(name = clientType(tpe.name))

  private def sourceFieldDefinition(typeName: String, field: String): Option[__Field] =
    sourceRootType.types.get(typeName).flatMap(tpe => Option(tpe.getFieldOrNull(field)))

}

private[gateway] object SchemaMapping {

  def compile(
    source: String,
    rootType: RootType,
    document: Document,
    federation: Boolean,
    transformations: List[SchemaTransformation]
  ): Either[List[String], SchemaMapping] = {
    val prefix         = s"[$source]"
    val operationRoots =
      rootType.queryType.name.toSet ++ rootType.mutationType.flatMap(_.name).toSet ++
        rootType.subscriptionType.flatMap(_.name).toSet
    val context        =
      ValidationContext(rootType.types, operationRoots, SchemaComposer.federationTransportTypes(document, federation))
    val changes        = transformations.map(normalize)
    val mappings       = changes.foldLeft(Mappings())(_.add(_))
    val renames        = changes.collect { case Change(coordinate, Some(renamed)) => coordinate -> renamed }

    val missing                    = changes.collect {
      case Change(coordinate, _) if !coordinate.exists(context) =>
        s"$prefix ${coordinate.display} does not exist."
    }
    val restrictions               = changes.flatMap(change => change.coordinate.restrictions(change, context, prefix))
    val hiddenReferences           = referencedHiddenInputFields(document, rootType, mappings.hiddenInputFieldSources)
    val hiddenInputFieldReferences =
      hiddenReferences.toList.sortBy(field => (field.typeName, field.fieldName)).map { case TypeField(tpe, field) =>
        s"$prefix Hidden input field '$tpe.$field' is referenced by a directive or default value."
      }
    val invalidRenameTargets       = renames.flatMap { case (coordinate, target) =>
      if (Parser.parseName(target).isLeft)
        List(s"$prefix ${coordinate.display} cannot be transformed to invalid GraphQL name '$target'.")
      else if (target.startsWith("__"))
        List(s"$prefix ${coordinate.display} cannot be transformed to reserved GraphQL name '$target'.")
      else Nil
    }
    val collisions                 = renames.flatMap { case (coordinate, target) => coordinate.collision(context, target, prefix) }
    val transformedCollisions      = renames.groupBy { case (coordinate, target) =>
      coordinate.targetScope -> target
    }.collect {
      case ((_, target), values) if values.map(_._1.id).distinct.size > 1 =>
        val coordinates = values.map(_._1).distinct.sortBy(_.id)
        val references  = coordinates.map(_.display.dropWhile(_ != '\'')).mkString(", ")
        s"$prefix ${coordinates.headOption.map(_.plural).getOrElse("Coordinates")} $references are both transformed to '$target'."
    }.toList
    val conflictingTransformations = changes
      .groupBy(_.coordinate.id)
      .collect {
        case (_, values) if values.map(_.renamed).distinct.size > 1 =>
          s"$prefix Coordinate ${values.headOption.map(_.coordinate.display.dropWhile(_ != '\'')).getOrElse("''")} has conflicting transformations."
      }
      .toList
    val diagnostics                =
      (missing ::: restrictions ::: hiddenInputFieldReferences ::: invalidRenameTargets :::
        collisions ::: transformedCollisions ::: conflictingTransformations).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else Right(new SchemaMapping(rootType, mappings))
  }

  private[composition] final case class InputReferences(
    inputTypes: Set[String] = Set.empty,
    enumValues: Set[(String, String)] = Set.empty,
    inputFields: Set[TypeField] = Set.empty
  ) {
    def ++(that: InputReferences): InputReferences =
      InputReferences(
        inputTypes ++ that.inputTypes,
        enumValues ++ that.enumValues,
        inputFields ++ that.inputFields
      )
  }

  private[composition] def inputReferences(tpe: __Type, value: InputValue): InputReferences = {
    def loop(expected: __Type, input: InputValue): InputReferences =
      expected.kind match {
        case __TypeKind.NON_NULL     => expected.ofType.fold(InputReferences())(loop(_, input))
        case __TypeKind.LIST         =>
          expected.ofType.fold(InputReferences()) { nested =>
            input match {
              case InputListValue(values) => values.foldLeft(InputReferences())(_ ++ loop(nested, _))
              case NullValue              => InputReferences()
              case singleton              => loop(nested, singleton)
            }
          }
        case __TypeKind.INPUT_OBJECT =>
          val typeName = expected.name.getOrElse("")
          val own      = InputReferences(inputTypes = Set(typeName))
          input match {
            case InputObjectValue(fields) =>
              fields.foldLeft(own) { case (result, (name, nested)) =>
                val fieldReference  = InputReferences(inputFields = Set(TypeField(typeName, name)))
                val nestedReference = expected.allInputFields
                  .find(_.name == name)
                  .fold(InputReferences())(field => loop(field._type, nested))
                result ++ fieldReference ++ nestedReference
              }
            case _                        => own
          }
        case __TypeKind.ENUM         =>
          val typeName  = expected.name.getOrElse("")
          val valueName = input match {
            case EnumValue(name)   => Some(name)
            case StringValue(name) => Some(name)
            case _                 => None
          }
          InputReferences(
            inputTypes = Set(typeName),
            enumValues = valueName.map(typeName -> _).toSet
          )
        case _                       =>
          InputReferences(inputTypes = expected.name.toSet)
      }

    loop(tpe, value)
  }

  private final case class DirectiveContext(
    names: SchemaComposer.FederationDirectiveNames,
    contextTypes: Map[ContextName, List[String]]
  )

  private final case class ValidationContext(
    types: Map[String, __Type],
    operationRoots: Set[String],
    transportTypes: Set[String]
  ) {
    def isFederation: Boolean = transportTypes.nonEmpty
  }

  /**
   * A schema element targeted by a rename or hide operation, such as Product.price or Query.product(id:).
   */
  private sealed trait Coordinate {
    def id: String
    def display: String
    def targetScope: String
    def plural: String
    final def exists(context: ValidationContext): Boolean = targetExists(context, currentName)
    def targetExists(context: ValidationContext, target: String): Boolean

    def restrictions(change: Change, context: ValidationContext, prefix: String): List[String]

    final def collision(context: ValidationContext, target: String, prefix: String): Option[String] =
      if (target != currentName && targetExists(context, target))
        Some(s"$prefix $display is transformed to existing ${targetDescription(target)}.")
      else None

    def currentName: String
    def targetDescription(target: String): String
  }

  private final case class TypeCoordinate(typeName: String) extends Coordinate {
    val id          = s"type:$typeName"
    val display     = s"Type '$typeName'"
    val targetScope = "type"
    val plural      = "Types"
    val currentName = typeName

    def targetExists(context: ValidationContext, target: String): Boolean = context.types.contains(target)
    def targetDescription(target: String): String                         = s"type '$target'"

    def restrictions(change: Change, context: ValidationContext, prefix: String): List[String] = {
      val operation =
        if (!context.operationRoots.contains(typeName)) Nil
        else
          change.renamed.fold(
            List(s"$prefix Operation root type '$typeName' cannot be hidden.")
          )(_ => List(s"$prefix Operation root type '$typeName' cannot be renamed."))
      val transport =
        if (context.transportTypes.contains(typeName))
          List(s"$prefix Federation transport type '$typeName' cannot be transformed.")
        else
          change.renamed
            .filter(context.transportTypes)
            .toList
            .map(target =>
              s"$prefix Type '$typeName' cannot be transformed to reserved Federation transport type '$target'."
            )
      operation ::: transport
    }
  }

  private final case class FieldCoordinate(typeName: String, fieldName: String) extends Coordinate {
    val id          = s"field:$typeName.$fieldName"
    val display     = s"Field '$typeName.$fieldName'"
    val targetScope = s"field:$typeName"
    val plural      = "Fields"
    val currentName = fieldName

    def targetExists(context: ValidationContext, target: String): Boolean =
      context.types.get(typeName).exists(_.allFields.exists(_.name == target))
    def targetDescription(target: String): String                         = s"field '$target'"

    def restrictions(change: Change, context: ValidationContext, prefix: String): List[String] = {
      val rootTransport = context.isFederation && context.operationRoots.contains(typeName)
      if (context.transportTypes.contains(typeName) || rootTransport && federationRootFields.contains(fieldName))
        List(s"$prefix Federation transport field '$typeName.$fieldName' cannot be transformed.")
      else
        change.renamed
          .filter(target => rootTransport && federationRootFields.contains(target))
          .toList
          .map(target =>
            s"$prefix Field '$typeName.$fieldName' cannot be transformed to reserved Federation transport field '$target'."
          )
    }
  }

  private final case class ArgumentCoordinate(typeName: String, fieldName: String, argumentName: String)
      extends Coordinate {
    val id          = s"argument:$typeName.$fieldName.$argumentName"
    val display     = s"Argument '$typeName.$fieldName($argumentName:)'"
    val targetScope = s"argument:$typeName.$fieldName"
    val plural      = "Arguments"
    val currentName = argumentName

    private def definition(context: ValidationContext, target: String = argumentName): Option[__InputValue] =
      context.types
        .get(typeName)
        .flatMap(tpe => Option(tpe.getFieldOrNull(fieldName)))
        .flatMap(_.allArgs.find(_.name == target))

    def targetExists(context: ValidationContext, target: String): Boolean =
      definition(context, target).nonEmpty
    def targetDescription(target: String): String                         = s"argument '$target'"

    def restrictions(change: Change, context: ValidationContext, prefix: String): List[String] = {
      val hidesRequired = change.renamed.isEmpty && definition(context).exists(isRequiredInput)
      val isTransport   = context.transportTypes.contains(typeName) ||
        context.isFederation && context.operationRoots.contains(typeName) && federationRootFields.contains(fieldName)
      check(!hidesRequired, s"$prefix Required argument '$typeName.$fieldName($argumentName:)' cannot be hidden.") :::
        check(
          !isTransport,
          s"$prefix Federation transport argument '$typeName.$fieldName($argumentName:)' cannot be transformed."
        )
    }
  }

  private final case class InputFieldCoordinate(typeName: String, fieldName: String) extends Coordinate {
    val id          = s"input:$typeName.$fieldName"
    val display     = s"Input field '$typeName.$fieldName'"
    val targetScope = s"input:$typeName"
    val plural      = "Input fields"
    val currentName = fieldName

    private def definition(context: ValidationContext): Option[__InputValue] =
      context.types.get(typeName).flatMap(_.allInputFields.find(_.name == fieldName))

    def targetExists(context: ValidationContext, target: String): Boolean =
      context.types.get(typeName).exists(_.allInputFields.exists(_.name == target))
    def targetDescription(target: String): String                         = s"field '$target'"

    def restrictions(change: Change, context: ValidationContext, prefix: String): List[String] = {
      val hidesRequired = change.renamed.isEmpty && definition(context).exists(isRequiredInput)
      check(!hidesRequired, s"$prefix Required input field '$typeName.$fieldName' cannot be hidden.") :::
        check(
          !context.transportTypes.contains(typeName),
          s"$prefix Federation transport input field '$typeName.$fieldName' cannot be transformed."
        )
    }
  }

  // A missing replacement name means the schema element is hidden.
  private final case class Change(coordinate: Coordinate, renamed: Option[String])

  private val federationRootFields = Set(EntitiesField, ServiceField)

  private final case class Mappings(
    typeNames: Map[String, String] = Map.empty,
    fieldNames: Map[TypeField, String] = Map.empty,
    argumentNames: Map[FieldArgument, String] = Map.empty,
    hiddenTypeSources: Set[String] = Set.empty,
    hiddenFieldSources: Set[TypeField] = Set.empty,
    hiddenArgumentSources: Set[FieldArgument] = Set.empty,
    hiddenInputFieldSources: Set[TypeField] = Set.empty
  ) {
    def renamesNothing: Boolean =
      typeNames.isEmpty && fieldNames.isEmpty && argumentNames.isEmpty

    def nonEmpty: Boolean =
      !renamesNothing || hiddenTypeSources.nonEmpty || hiddenFieldSources.nonEmpty || hiddenArgumentSources.nonEmpty ||
        hiddenInputFieldSources.nonEmpty

    def add(change: Change): Mappings =
      (change.coordinate, change.renamed) match {
        case (TypeCoordinate(name), Some(renamed))                 => copy(typeNames = typeNames.updated(name, renamed))
        case (TypeCoordinate(name), None)                          => copy(hiddenTypeSources = hiddenTypeSources + name)
        case (FieldCoordinate(tpe, name), Some(renamed))           =>
          copy(fieldNames = fieldNames.updated(TypeField(tpe, name), renamed))
        case (FieldCoordinate(tpe, name), None)                    =>
          copy(hiddenFieldSources = hiddenFieldSources + TypeField(tpe, name))
        case (ArgumentCoordinate(tpe, field, name), Some(renamed)) =>
          copy(argumentNames = argumentNames.updated(FieldArgument(tpe, field, name), renamed))
        case (ArgumentCoordinate(tpe, field, name), None)          =>
          copy(hiddenArgumentSources = hiddenArgumentSources + FieldArgument(tpe, field, name))
        case (InputFieldCoordinate(tpe, name), _)                  =>
          copy(hiddenInputFieldSources = hiddenInputFieldSources + TypeField(tpe, name))
      }

    def renamedType(name: String): String               = typeNames.getOrElse(name, name)
    def renamedField(tpe: String, name: String): String = fieldNames.getOrElse(TypeField(tpe, name), name)

    def hiddenTypes: Set[String]            = hiddenTypeSources.map(renamedType)
    def hiddenFields: Set[TypeField]        = hiddenFieldSources.map { case TypeField(tpe, name) =>
      TypeField(renamedType(tpe), renamedField(tpe, name))
    }
    def hiddenArguments: Set[FieldArgument] = hiddenArgumentSources.map { argument =>
      FieldArgument(
        renamedType(argument.typeName),
        renamedField(argument.typeName, argument.fieldName),
        argumentNames.getOrElse(argument, argument.argumentName)
      )
    }
    def hiddenInputFields: Set[TypeField]   = hiddenInputFieldSources.map { case TypeField(tpe, name) =>
      TypeField(renamedType(tpe), name)
    }

  }

  private def normalize(transformation: SchemaTransformation): Change =
    transformation match {
      case RenameType(name, renamed)                 => Change(TypeCoordinate(name), Some(renamed))
      case HideType(name)                            => Change(TypeCoordinate(name), None)
      case RenameField(tpe, name, renamed)           => Change(FieldCoordinate(tpe, name), Some(renamed))
      case HideField(tpe, name)                      => Change(FieldCoordinate(tpe, name), None)
      case RenameArgument(tpe, field, name, renamed) =>
        Change(ArgumentCoordinate(tpe, field, name), Some(renamed))
      case HideArgument(tpe, field, name)            => Change(ArgumentCoordinate(tpe, field, name), None)
      case HideInputField(tpe, name)                 => Change(InputFieldCoordinate(tpe, name), None)
    }

  private def referencedHiddenInputFields(
    document: Document,
    rootType: RootType,
    hiddenInputFields: Set[TypeField]
  ): Set[TypeField] =
    if (hiddenInputFields.isEmpty) Set.empty
    else {
      def directiveValues(directives: List[Directive]): List[(__Type, InputValue)] =
        directives.flatMap { directive =>
          rootType.additionalDirectives.find(_.name == directive.name).toList.flatMap { definition =>
            directive.arguments.toList.flatMap { case (name, value) =>
              definition.allArgs.find(_.name == name).map(argument => argument._type -> value)
            }
          }
        }

      def inputValues(value: __InputValue): List[(__Type, InputValue)] =
        directiveValues(value.directives.getOrElse(Nil)) ::: value.parsedDefaultValue.map(value._type -> _).toList

      val schemaDirectives = DirectiveComposition.schemaDirectives(document)
      val typeValues       = rootType.types.valuesIterator.flatMap { tpe =>
        directiveValues(tpe.directives.getOrElse(Nil)) :::
          tpe.allFields.flatMap(field =>
            directiveValues(field.directives.getOrElse(Nil)) ::: field.allArgs.flatMap(inputValues)
          ) ::: tpe.allInputFields.flatMap(inputValues) :::
          tpe.allEnumValues.flatMap(value => directiveValues(value.directives.getOrElse(Nil)))
      }
      val definitionValues = rootType.additionalDirectives.iterator.flatMap(_.allArgs.flatMap(inputValues))
      val found            = (directiveValues(schemaDirectives).iterator ++ typeValues ++ definitionValues).map {
        case (tpe, value) => inputReferences(tpe, value)
      }
        .foldLeft(InputReferences())(_ ++ _)

      found.inputFields.filter(hiddenInputFields)
    }
}
