package caliban.gateway.internal.composition

import caliban.execution.Field
import caliban.gateway._
import caliban.InputValue
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.gateway.internal.planning.OperationPlan.RequiredSelection
import caliban.gateway.internal.composition.ComposedGraph.ContextName
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective
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
  private[composition] val sourceRootType: RootType,
  mappings: SchemaMapping.Mappings
) {
  import SchemaMapping._

  val nonEmpty: Boolean = mappings.nonEmpty

  private[internal] val typeNames: Map[String, String] =
    mappings.rootNames ++ mappings.renames.collect { case (TypeTarget(name), renamed) => name -> renamed }

  val hidden: Set[Coordinate] = mappings.hidden.flatMap {
    case TypeTarget(tpe)                  =>
      sourceRootType.types.get(tpe).map(value => TypeCoordinate(clientType(tpe), typeLocation(value.kind)))
    case FieldTarget(tpe, field)          => Some(FieldCoordinate(clientType(tpe), clientField(tpe, field)))
    case ArgumentTarget(tpe, field, name) =>
      Some(ArgumentCoordinate(clientType(tpe), clientField(tpe, field), clientArgument(tpe, field, name)))
    case InputFieldTarget(tpe, field)     => Some(InputFieldCoordinate(clientType(tpe), field))
  }

  def clientType(name: String): String =
    typeNames.getOrElse(name, name)

  def sourceType(name: String): String =
    sourceTypes.getOrElse(name, name)

  def transform(document: Document, directiveNames: FederationCompilation.FederationDirectiveNames): Document = {
    val contexts = document.typeDefinitions.flatMap { tpe =>
      tpe.directives.filter(directiveNames.is(_, FederationDirective.Context)).flatMap { directive =>
        stringArgument(directive.arguments, "name").map(ContextName(_)).toList.flatMap { name =>
          sourceRootType.types.get(tpe.name).toList.flatMap(_.possibleTypeNames.toList.sorted).map(name -> _)
        }
      }
    }.groupMap(_._1)(_._2).map { case (name, declarations) => name -> declarations.distinct }
    new DocumentTransform(directiveNames, contexts).transform(document)
  }

  def fieldToSource(field: Field): Field = {
    val clientParent       = innerParentTypeName(field)
    val sourceParent       = sourceType(clientParent)
    val sourceName         = sourceField(clientParent, field.name)
    val composedDirectives = field.parentType
      .flatMap(parent => fieldDefinition(parent.innerType, field.name))
      .flatMap(_.directives)
      .getOrElse(Nil)
    val arguments          = field.arguments.map { case (name, value) =>
      sourceArguments.getOrElse(ArgumentCoordinate(clientParent, field.name, name), name) -> value
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

  def representationToSource(typeName: String, value: InputObjectValue): InputObjectValue =
    if (mappings.renamesNothing) value
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

  private val sourceTypes     = typeNames.map(_.swap)
  private val sourceFields    = mappings.renames.collect { case (FieldTarget(tpe, field), renamed) =>
    FieldCoordinate(clientType(tpe), renamed) -> field
  }
  private val sourceArguments = mappings.renames.collect { case (ArgumentTarget(tpe, field, argument), renamed) =>
    ArgumentCoordinate(clientType(tpe), clientField(tpe, field), renamed) -> argument
  }

  private[composition] def clientField(typeName: String, field: String): String =
    mappings.renames.getOrElse(FieldTarget(typeName, field), field)

  private def sourceField(typeName: String, field: String): String =
    sourceFields.getOrElse(FieldCoordinate(typeName, field), field)

  private def clientArgument(typeName: String, field: String, argument: String): String =
    mappings.renames.getOrElse(ArgumentTarget(typeName, field, argument), argument)

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

  private final class DocumentTransform(
    names: FederationCompilation.FederationDirectiveNames,
    contextTypes: Map[ContextName, List[String]]
  ) {
    def transform(document: Document): Document =
      Document(document.definitions.map(transformDefinition), document.sourceMapper)

    private def transformDefinition(definition: Definition): Definition =
      definition match {
        case value: SchemaDefinition          =>
          value.copy(
            query = value.query.map(clientType),
            mutation = value.mutation.map(clientType),
            subscription = value.subscription.map(clientType)
          )
        case value: DirectiveDefinition       =>
          value.copy(args = value.args.map(transformInputValueDefinition))
        case value: ObjectTypeDefinition      =>
          value.copy(
            name = clientType(value.name),
            implements = value.implements.map(transformNamedType),
            directives = transformKeys(value.name, value.directives),
            fields = value.fields.map(transformFieldDefinition(value.name, _))
          )
        case value: InterfaceTypeDefinition   =>
          value.copy(
            name = clientType(value.name),
            implements = value.implements.map(transformNamedType),
            directives = transformKeys(value.name, value.directives),
            fields = value.fields.map(transformFieldDefinition(value.name, _))
          )
        case value: InputObjectTypeDefinition =>
          value.copy(name = clientType(value.name), fields = value.fields.map(transformInputValueDefinition))
        case value: EnumTypeDefinition        => value.copy(name = clientType(value.name))
        case value: UnionTypeDefinition       =>
          value.copy(name = clientType(value.name), memberTypes = value.memberTypes.map(clientType))
        case value: ScalarTypeDefinition      => value.copy(name = clientType(value.name))
        case other                            => other
      }

    private def transformKeys(typeName: String, directives: List[Directive]): List[Directive] =
      directives.map(directive =>
        if (names.is(directive, FederationDirective.Key)) transformFieldSet(directive, typeName) else directive
      )

    private def transformFieldDefinition(typeName: String, field: FieldDefinition): FieldDefinition = {
      val outputType = Type.innerType(field.ofType)
      field.copy(
        name = clientField(typeName, field.name),
        args = field.args.map { argument =>
          transformInputValueDefinition(argument).copy(
            name = clientArgument(typeName, field.name, argument.name),
            directives = argument.directives.map(directive =>
              if (names.is(directive, FederationDirective.FromContext)) transformFromContext(directive) else directive
            )
          )
        },
        ofType = transformType(field.ofType),
        directives = field.directives.map { directive =>
          names.resolved.get(directive.name) match {
            // @provides selects on the return type; @key and @requires select on the parent type.
            case Some(FederationDirective.Requires) => transformFieldSet(directive, typeName)
            case Some(FederationDirective.Provides) => transformFieldSet(directive, outputType)
            case Some(FederationDirective.ListSize) => transformListSize(directive, typeName, field.name, outputType)
            case _                                  => directive
          }
        }
      )
    }

    private def transformFromContext(directive: Directive): Directive =
      stringArgument(directive.arguments, "field")
        .flatMap(ContextCompilation.parseSelection)
        .fold(directive) { case (name, selections) =>
          val rewritten = contextTypes.getOrElse(name, Nil).map { parent =>
            parent -> selections.map(transformFieldSetSelection(parent, _))
          }
          val fields    = rewritten match {
            case Nil                                                => selections
            case (_, first) :: _ if rewritten.forall(_._2 == first) => first
            case (_, first) :: _                                    =>
              val fragments = first.collect { case fragment: Selection.InlineFragment => fragment }
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

    private def transformInputValueDefinition(value: InputValueDefinition): InputValueDefinition =
      value.copy(ofType = transformType(value.ofType))
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
          val (argument, rest) = path.span(_ != '.')
          clientArgument(parent, field, argument) + rest
        }
      case ("sizedFields", value)      =>
        "sizedFields" -> mapStrings(value) { selection =>
          parseFieldSet(selection)
            .fold(selection)(fields => renderFieldSet(fields.map(transformFieldSetSelection(outputType, _))))
        }
      case other                       => other
    })
  }

  private def transformFieldSet(directive: Directive, parentType: String): Directive =
    stringArgument(directive.arguments, "fields").flatMap(parseFieldSet).fold(directive) { selections =>
      val fields = renderFieldSet(selections.map(transformFieldSetSelection(parentType, _)))
      directive.copy(arguments = directive.arguments.updated("fields", StringValue(fields)))
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

  private def transformType(tpe: Type): Type =
    tpe match {
      case NamedType(name, nonNull)  => NamedType(clientType(name), nonNull)
      case ListType(ofType, nonNull) => ListType(transformType(ofType), nonNull)
    }

  private def transformNamedType(tpe: NamedType): NamedType =
    tpe.copy(name = clientType(tpe.name))

  private def sourceFieldDefinition(typeName: String, field: String): Option[__Field] =
    sourceRootType.types.get(typeName).flatMap(fieldDefinition(_, field))

}

private[gateway] object SchemaMapping {

  def compile(
    rootType: RootType,
    names: FederationCompilation.FederationDirectiveNames,
    transformations: List[SchemaTransformation]
  ): Either[List[String], SchemaMapping] = {
    val roots    = List(
      Some(QueryRoot -> rootType.queryType),
      rootType.mutationType.map("Mutation" -> _),
      rootType.subscriptionType.map("Subscription" -> _)
    ).flatten.flatMap { case (operation, tpe) => tpe.name.map(_ -> operation) }.toMap
    val context  = ValidationContext(rootType.types, roots.keySet, names.hiddenTypes)
    val renames  = transformations.flatMap(change => change.renamed.map(change.coordinate -> _))
    val mappings = Mappings(
      roots.filter { case (source, root) => source != root },
      renames.toMap,
      transformations.collect { case change if change.renamed.isEmpty => change.coordinate }.toSet
    )

    val missing                    = transformations.collect {
      case change if !change.coordinate.exists(context) => s"${change.coordinate.display} does not exist."
    }
    val restrictions               = transformations.flatMap(change => change.coordinate.restrictions(change, context))
    val invalidRenameTargets       = renames.flatMap { case (coordinate, target) =>
      if (Parser.parseName(target).isLeft)
        List(s"${coordinate.display} cannot be transformed to invalid GraphQL name '$target'.")
      else if (target.startsWith("__"))
        List(s"${coordinate.display} cannot be transformed to reserved GraphQL name '$target'.")
      else Nil
    }
    val collisions                 = renames.flatMap { case (coordinate, target) => coordinate.collision(context, target) }
    val transformedCollisions      = renames.groupBy { case (coordinate, target) => coordinate.renamed(target) }.collect {
      case (renamed, values) if values.map(_._1).distinct.size > 1 =>
        val references = values.map(_._1).distinct.sortBy(_.currentName).map(_.reference).mkString(", ")
        s"${renamed.plural} $references are both transformed to '${renamed.currentName}'."
    }.toList
    val conflictingTransformations = transformations
      .groupBy(_.coordinate)
      .collect {
        case (coordinate, values) if values.map(_.renamed).distinct.size > 1 =>
          s"Coordinate ${coordinate.reference} has conflicting transformations."
      }
      .toList
    val diagnostics                =
      (missing ::: restrictions ::: invalidRenameTargets :::
        collisions ::: transformedCollisions ::: conflictingTransformations).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else Right(new SchemaMapping(rootType, mappings))
  }

  private[composition] def inputReferences(tpe: __Type, value: InputValue): Set[Coordinate] =
    (tpe.kind, value) match {
      case (__TypeKind.NON_NULL, _)                            => tpe.ofType.fold(Set.empty[Coordinate])(inputReferences(_, value))
      case (__TypeKind.LIST, NullValue)                        => Set.empty
      case (__TypeKind.LIST, InputListValue(values))           =>
        tpe.ofType.fold(Set.empty[Coordinate])(nested => values.flatMap(inputReferences(nested, _)).toSet)
      case (__TypeKind.LIST, singleton)                        => tpe.ofType.fold(Set.empty[Coordinate])(inputReferences(_, singleton))
      case (__TypeKind.INPUT_OBJECT, InputObjectValue(fields)) =>
        fields.toSet[(String, InputValue)].flatMap { case (name, nested) =>
          inputFieldDefinition(tpe, name).fold(Set.empty[Coordinate])(field => inputReferences(field._type, nested)) ++
            tpe.name.map(InputFieldCoordinate(_, name))
        } ++ namedReference(tpe)
      case (__TypeKind.ENUM, EnumValue(name))                  => namedReference(tpe) ++ tpe.name.map(EnumValueCoordinate(_, name))
      case (__TypeKind.ENUM, StringValue(name))                => namedReference(tpe) ++ tpe.name.map(EnumValueCoordinate(_, name))
      case _                                                   => namedReference(tpe)
    }

  private[composition] def namedReference(tpe: __Type): Set[Coordinate] =
    tpe.name.map(name => TypeCoordinate(name, typeLocation(tpe.kind)): Coordinate).toSet

  private[gateway] final case class ValidationContext(
    types: Map[String, __Type],
    operationRoots: Set[String],
    transportTypes: Set[String]
  ) {
    def transportField(typeName: String, fieldName: String): Boolean =
      transportTypes(typeName) || transportTypes.nonEmpty && operationRoots(typeName) && TransportFields(fieldName)
  }

  /**
   * A schema element targeted by a rename or hide operation, such as Product.price or Query.product(id:).
   */
  private[gateway] sealed abstract class Target(kind: String, val plural: String) {
    def currentName: String
    def reference: String
    def renamed(name: String): Target
    def exists(context: ValidationContext): Boolean
    protected def transport(context: ValidationContext): Boolean
    protected def reserved(target: String, context: ValidationContext): Boolean = false
    protected def input(context: ValidationContext): Option[__InputValue]       = None
    protected def operationRoot(context: ValidationContext): Boolean            = false

    final def display: String = s"$kind $reference"

    final def restrictions(change: SchemaTransformation, context: ValidationContext): List[String] = {
      val element = s"${kind.toLowerCase} $reference"
      check(
        !operationRoot(context),
        s"Operation root $element cannot be ${change.renamed.fold("hidden")(_ => "renamed")}."
      ) :::
        check(
          change.renamed.nonEmpty || !input(context).exists(isRequiredInput),
          s"Required $element cannot be hidden."
        ) :::
        (if (transport(context)) List(s"Federation transport $element cannot be transformed.")
         else
           change.renamed.toList.collect {
             case target if reserved(target, context) =>
               s"$display cannot be transformed to reserved Federation transport ${kind.toLowerCase} '$target'."
           })
    }

    final def collision(context: ValidationContext, target: String): Option[String] =
      if (target != currentName && renamed(target).exists(context))
        Some(s"$display is transformed to existing ${kind.toLowerCase} '$target'.")
      else None
  }

  private[gateway] final case class TypeTarget(typeName: String) extends Target("Type", "Types") {
    val currentName = typeName
    val reference   = s"'$typeName'"

    def renamed(name: String): Target               = TypeTarget(name)
    def exists(context: ValidationContext): Boolean = context.types.contains(typeName)

    protected def transport(context: ValidationContext)                         = context.transportTypes(typeName)
    override protected def reserved(target: String, context: ValidationContext) = context.transportTypes(target)
    override protected def operationRoot(context: ValidationContext)            = context.operationRoots(typeName)
  }

  private[gateway] final case class FieldTarget(typeName: String, fieldName: String) extends Target("Field", "Fields") {
    val currentName = fieldName
    val reference   = s"'$typeName.$fieldName'"

    def renamed(name: String): Target               = FieldTarget(typeName, name)
    def exists(context: ValidationContext): Boolean =
      context.types.get(typeName).flatMap(fieldDefinition(_, fieldName)).nonEmpty

    protected def transport(context: ValidationContext)                         = context.transportField(typeName, fieldName)
    override protected def reserved(target: String, context: ValidationContext) =
      context.transportField(typeName, target)
  }

  private[gateway] final case class ArgumentTarget(typeName: String, fieldName: String, argumentName: String)
      extends Target("Argument", "Arguments") {
    val currentName = argumentName
    val reference   = s"'$typeName.$fieldName($argumentName:)'"

    override protected def input(context: ValidationContext): Option[__InputValue] =
      context.types
        .get(typeName)
        .flatMap(fieldDefinition(_, fieldName))
        .flatMap(_.allArgs.find(_.name == argumentName))

    def renamed(name: String): Target                   = ArgumentTarget(typeName, fieldName, name)
    def exists(context: ValidationContext): Boolean     = input(context).nonEmpty
    protected def transport(context: ValidationContext) = context.transportField(typeName, fieldName)
  }

  private[gateway] final case class InputFieldTarget(typeName: String, fieldName: String)
      extends Target("Input field", "Input fields") {
    val currentName = fieldName
    val reference   = s"'$typeName.$fieldName'"

    override protected def input(context: ValidationContext): Option[__InputValue] =
      context.types.get(typeName).flatMap(inputFieldDefinition(_, fieldName))

    def renamed(name: String): Target                   = InputFieldTarget(typeName, name)
    def exists(context: ValidationContext): Boolean     = input(context).nonEmpty
    protected def transport(context: ValidationContext) = context.transportTypes(typeName)
  }

  private val QueryRoot = "Query"

  private final case class Mappings(rootNames: Map[String, String], renames: Map[Target, String], hidden: Set[Target]) {
    def renamesNothing: Boolean = renames.isEmpty

    def nonEmpty: Boolean = renames.nonEmpty || rootNames.nonEmpty || hidden.nonEmpty
  }
}
