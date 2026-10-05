package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.CompositionDiagnostic.{ error, Code }
import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective._
import caliban.gateway.internal.composition.SchemaComposer.SubgraphKeys
import caliban.gateway.internal.composition.TypeComposition.{ FieldOverride, RootOperations, SubgraphMode }
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType

import scala.collection.compat._

/**
 * Immutable schema and ownership metadata produced by composition.
 */
private[gateway] final case class ComposedGraph private[internal] (
  val rootType: RootType,
  val fieldRoutes: Map[FieldCoordinate, List[Source]],
  val progressiveRoutes: Map[FieldCoordinate, ProgressiveRoute],
  val sources: List[Source],
  val possibleTypesByName: Map[String, Set[String]],
  val costMetadata: CostMetadata,
  val securityApplications: List[SecurityDirectiveApplication]
) {
  def resolveOverrides(activeOverrides: Set[OverrideLabel]): ComposedGraph =
    copy(
      fieldRoutes = fieldRoutes ++ progressiveRoutes.collect {
        case (coordinate, route) if activeOverrides(route.label) => coordinate -> route.sources
      },
      progressiveRoutes = Map.empty
    )

  def hasProgressiveOverrides: Boolean = progressiveRoutes.nonEmpty

  def securityAt(
    typeName: String,
    fieldName: Option[String],
    condition: Option[Set[String]]
  ): List[SecurityDirectiveApplication] =
    securityByField
      .getOrElse(fieldName, Nil)
      .filter(application =>
        application.typeName == typeName ||
          typesOverlap(possibleTypesByName, typeName, application.typeName, condition)
      )

  private lazy val securityByField = securityApplications.groupBy(_.fieldName)

  private lazy val progressiveLabels =
    progressiveRoutes.toList.groupMap(_._1.fieldName)(route => route._1.typeName -> route._2.label)

  def progressiveOverrides(typeName: String, field: String): List[OverrideLabel] =
    progressiveLabels.getOrElse(field, Nil).collect {
      case (owner, label) if typesOverlap(possibleTypesByName, typeName, owner, None) => label
    }

  def operationRoot(operation: OperationType): Option[__Type] = rootType.types.get(rootName(operation))

  def rootFieldSources(operation: OperationType, field: String): List[Source] = {
    val sources   = fieldRoutes.getOrElse(FieldCoordinate(rootName(operation), field), Nil)
    val composite = operationRoot(operation)
      .flatMap(fieldDefinition(_, field))
      .exists(definition => isCompositeType(definition._type.innerType))
    if (composite) sources else sources.take(1)
  }

  def fieldSources(typeName: String, field: String, preferred: Source): List[Source] =
    preferredFirst(fieldRoutes.getOrElse(FieldCoordinate(typeName, field), Nil))(_ == preferred)

  def interfaceObjectFieldSources(typeName: String, field: String, preferred: Source): List[SourceType] =
    if (fieldRoutes.contains(FieldCoordinate(typeName, field))) Nil
    else
      preferredFirst(interfaceObjects(typeName).filter(key => ownsField(key.source, key.typeName, field)))(
        _.source == preferred
      )

  // Entity lookups allow switching sources. Otherwise, keep the current sources when any can resolve the field.
  def candidateSources(current: Set[Source], parentType: String, field: String): Set[Source] = {
    val routed = fieldRoutes.getOrElse(FieldCoordinate(parentType, field), Nil).toSet
    if (routed.isEmpty) current
    else if (sources.exists(_.entityLookups(parentType).nonEmpty)) routed
    else {
      val constrained = current intersect routed
      if (constrained.isEmpty) routed else constrained
    }
  }

  def typenameSource(typeName: String, preferred: Source): Option[Source] =
    preferredFirst(
      sources.filter(source => source.entityLookups(typeName).nonEmpty && !source.isInterfaceObject(typeName))
    )(
      _ == preferred
    ).headOption

  def sourcesDefiningKey(typeName: String, fields: List[KeyField]): List[Source] =
    sources.filter(source => fields.forall(source.definesKeyField(typeName, _)))

  def ownsField(source: Source, typeName: String, field: String): Boolean =
    fieldRoutes.getOrElse(FieldCoordinate(typeName, field), Nil).contains(source)

  val hasContextArguments: Boolean = sources.exists(_.contexts.contextBindings.nonEmpty)

  def contextDeclarations(typeName: String): List[(Source, ContextDeclaration)] = {
    val selectedTypes = possibleTypesByName.getOrElse(typeName, Set(typeName))
    sources.flatMap(source => source.contexts.declarations.map(source -> _)).filter { case (_, declared) =>
      possibleTypesByName.getOrElse(declared.typeName, Set(declared.typeName)).exists(selectedTypes)
    }
  }

  def possibleReturnTypes(
    possibleSources: Set[Source],
    source: Source,
    parentType: String,
    field: String,
    outputType: String
  ): Set[String] = {
    val candidates =
      if (possibleSources.nonEmpty) possibleSources.iterator
      else Iterator.single(source)
    val available  = candidates.flatMap { candidate =>
      candidate
        .fieldTypeName(parentType, field)
        .map(candidate.possibleTypes)
        .filter(_.nonEmpty)
    }
    // A selection planned across candidate sources can rely only on concrete return types shared by them.
    available
      .reduceOption(_ intersect _)
      .getOrElse(source.possibleTypes(outputType))
  }

  def interfaceObjectTypes(source: Source, typeName: String): List[__Type] =
    interfaceObjects(typeName).collect { case SourceType(`source`, name) => name }.flatMap(rootType.types.get)

  def isObjectType(typeName: String): Boolean =
    rootType.types.get(typeName).exists(_.kind == __TypeKind.OBJECT)

  def acceptsRuntimeType(typeName: String, runtimeType: String): Boolean =
    runtimeType == typeName || possibleTypesByName.getOrElse(typeName, Set.empty).contains(runtimeType)

  private def interfaceObjects(typeName: String): List[SourceType] = {
    val interfaces = rootType.types.get(typeName).filter(_.kind == __TypeKind.OBJECT).toList
    val names      = interfaces.flatMap(_.interfaces().getOrElse(Nil).flatMap(_.name)).sorted
    sources.flatMap(source => names.filter(source.isInterfaceObject).map(SourceType(source, _)))
  }

  private def preferredFirst[A](values: List[A])(preferred: A => Boolean): List[A] = {
    val (first, rest) = values.partition(preferred)
    first ::: rest
  }
}

private[gateway] object ComposedGraph {
  def rootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => "Query"
      case OperationType.Mutation     => "Mutation"
      case OperationType.Subscription => "Subscription"
    }

  /**
   * One composed subgraph: its schema mapping and the per-type metadata planning and execution read from it.
   */
  final class Source private[composition] (
    val name: String,
    val rootType: RootType,
    val schemaDirectives: List[Directive],
    val lookups: List[Lookup],
    val mapping: SchemaMapping,
    val federation1ExtensionTypes: Set[String],
    val directiveNames: FederationDirectiveNames
  ) {
    override def toString: String = name

    private def types: Map[String, __Type] = rootType.types

    lazy val directiveApplications: List[TypeSystemDirectiveApplication] =
      FederationCompilation.directiveApplications(rootType, schemaDirectives)

    lazy val keys: SubgraphKeys = SchemaComposer.subgraphKeys(this)

    def federation: Boolean = directiveNames.mode != SubgraphMode.Ordinary

    def invalidApplication[A](application: FederationApplication, code: Code)(
      result: Either[String, A]
    ): Either[CompositionDiagnostic, A] =
      result.left.map(message =>
        error(code, List(name), application.coordinate.schemaCoordinate)(
          s"Invalid Federation @${application.member.name} application at '${application.coordinate.display}': $message"
        )
      )

    lazy val (federationErrors, federationApplications) =
      (for {
        application <- directiveApplications
        directive   <- application.directives
        coordinate   = application.coordinate
        result      <-
          directiveNames.resolved.get(directive.name).map(validateApplication(name, coordinate, _, directive)) ++
            directiveNames.unsupported.get(directive.name).map(unsupported => Left(List(unsupported(name, coordinate))))
      } yield result).partitionMap(identity)

    lazy val repeatedDirectives: List[CompositionDiagnostic] = {
      val repeatable =
        rootType.additionalDirectives.map(definition => definition.name -> definition.isRepeatable).toMap ++
          directiveNames.resolved.map { case (local, member) => local -> member.definition.isRepeatable }
      directiveApplications.flatMap { application =>
        duplicates(application.directives.map(_.name))
          .filter(repeatable.get(_).contains(false))
          .map(directive =>
            error(Code.InvalidGraphQL, List(name), application.coordinate.schemaCoordinate)(
              s"Non-repeatable directive '@$directive' is applied more than once at '${application.coordinate.display}'."
            )
          )
      }
    }

    def applications(member: FederationDirective): List[FederationApplication] =
      federationApplications.filter(_.member == member)

    private lazy val appliedAt: Set[(FederationDirective, Coordinate)] =
      federationApplications.map(application => application.member -> application.coordinate).toSet

    def applied(member: FederationDirective, coordinate: Coordinate): Boolean = appliedAt(member -> coordinate)

    lazy val overrides: Map[Coordinate, (FieldOverride, Option[CompositionDiagnostic])] =
      applications(Override)
        .map(application => application.coordinate -> SchemaComposer.fieldOverride(this, application))
        .toMap

    lazy val compiledLookups: List[Either[List[CompositionDiagnostic], (String, EntityLookup)]] =
      lookups.map(lookup =>
        LookupCompilation
          .compile(this, lookup)
          .map { operation =>
            val keys = lookup.keyFields.map(field => KeyField(mapping.clientField(lookup.typeName, field), Nil))
            mapping.clientType(lookup.typeName) -> EntityLookup(keys, operation)
          }
      )

    private lazy val interfaceObjects: Set[String] =
      applications(InterfaceObject).collect { case FederationApplication(TypeCoordinate(typeName, _), _, _) =>
        typeName
      }.toSet -- RootOperations.keySet

    def isInterfaceObject(typeName: String): Boolean = interfaceObjects.contains(typeName)

    private lazy val entityLookupsByType: Map[String, List[EntityLookup]] =
      (if (!federation) compiledLookups.collect { case Right(lookup) => lookup }
       else
         keys.all.collect { case SchemaComposer.FederationKey(typeName, fields, true) =>
           typeName -> EntityLookup(fields, LookupOperation.FederationEntities)
         }).filterNot(lookup => RootOperations.contains(lookup._1)).groupMap(_._1)(_._2)

    def entityLookups(typeName: String): List[EntityLookup] = entityLookupsByType.getOrElse(typeName, Nil)

    lazy val (fieldSetErrors, requiredFieldSets, providedFieldSets) = {
      val (requiredErrors, required) = fieldSets(Requires, Code.RequiresInvalidFields)
      val (providedErrors, provided) = fieldSets(Provides, Code.ProvidesInvalidFields)
      (requiredErrors ::: providedErrors, required.toMap, provided.toMap)
    }

    private def fieldSets(
      member: FederationDirective,
      code: Code
    ): (List[CompositionDiagnostic], List[(FieldCoordinate, List[Selection])]) =
      applications(member).flatMap {
        case application @ FederationApplication(at @ FieldCoordinate(typeName, fieldName), _, _) =>
          for {
            parent <- types.get(typeName)
            field  <- fieldDefinition(parent, fieldName)
            target  = if (member == Provides) field._type.innerType else parent
          } yield SchemaComposer.validateFieldSet(this, application, target, code)(Right(_)).map(at -> _)
        case _                                                                                    => None
      }.partitionMap(identity)

    lazy val (contextErrors, contexts) =
      ContextCompilation.compile(this).fold(_ -> ContextCompilation.FederationContexts(Nil, Map.empty), Nil -> _)

    lazy val costs: Either[List[CompositionDiagnostic], CostMetadata] = CostCompilation.compile(this)

    lazy val diagnostics: List[CompositionDiagnostic] =
      keys.diagnostics ::: overrides.values.toList.flatMap(_._2) ::: federationErrors.flatten :::
        repeatedDirectives ::: LookupCompilation.declarationDiagnostics(this) :::
        compiledLookups.flatMap(_.left.getOrElse(Nil)) ::: fieldSetErrors ::: contextErrors :::
        costs.left.getOrElse(Nil)

    lazy val hidden: Set[Coordinate] =
      mapping.hidden ++ (applications(Inaccessible) ::: applications(FromContext)).map(_.coordinate)

    def sourceField(typeName: String, field: String): Option[__Field] =
      types.get(typeName).flatMap(fieldDefinition(_, field))

    def fieldTypeName(typeName: String, field: String): Option[String] =
      sourceField(typeName, field).flatMap(_._type.innerType.name)

    def definesKeyField(typeName: String, key: KeyField): Boolean =
      sourceField(typeName, key.name).exists { definition =>
        key.children.isEmpty || definition._type.innerType.name.exists(name =>
          key.children.forall(definesKeyField(name, _))
        )
      }

    def requiredFieldSet(typeName: String, field: String): List[Selection] =
      requiredFieldSets.getOrElse(FieldCoordinate(typeName, field), Nil)

    def providedFieldSet(typeName: String, field: String): List[Selection] =
      providedFieldSets.getOrElse(FieldCoordinate(typeName, field), Nil)

    def contextArguments(typeName: String, field: String): List[ContextArgument] =
      contexts.contextBindings.getOrElse(FieldCoordinate(typeName, field), Nil)

    def possibleTypes(typeName: String): Set[String] =
      types.get(typeName).fold(Set.empty[String])(_.possibleTypeNames)

    def fieldApplies(parentType: String, field: Field): Boolean =
      field._condition.forall(possibleTypes(parentType).exists(_))

    def prepareFields(parentType: String, fields: List[Field]): List[Field] =
      aliasConflicts(fields.map(prepareField(Some(parentType), _)))

    private def prepareField(parentType: Option[String], field: Field): Field = {
      val parent      = parentType.getOrElse(innerParentTypeName(field))
      // An @interfaceObject source sees one object, so fragments targeting concrete implementations must be removed.
      val targets     =
        if (isInterfaceObject(parent)) None
        else
          field.targets.map(original =>
            field._condition.fold(original)(
              _.filter(possibleTypes(parent)).filter(name => possibleTypes(name).contains(name))
            )
          )
      val childParent = fieldTypeName(parent, field.name).orElse(field.fieldType.innerType.name)
      val children    = aliasConflicts(field.fields.map(prepareField(childParent, _)))
      field.copy(targets = targets, fields = children)
    }

    private def aliasConflicts(fields: List[Field]): List[Field] = {
      def typeSignatures(field: Field): Set[String] = {
        val sourceDefinitions = field.targets.iterator
          .flatMap(_.iterator)
          .flatMap(target => sourceField(target, field.name))
          .map(value => DocumentRenderer.renderTypeName(value._type))
          .toSet
        if (sourceDefinitions.nonEmpty) sourceDefinitions else Set(DocumentRenderer.renderTypeName(field.fieldType))
      }

      val conflicts = fields
        .groupBy(_.aliasedName)
        .collect {
          case (name, values) if values.iterator.flatMap(typeSignatures).toSet.size > 1 => name
        }
        .toSet
      if (conflicts.isEmpty) fields
      else {
        val aliases = new PrivateAliases(responseNames(fields))
        fields.map { field =>
          if (!conflicts.contains(field.aliasedName)) field
          else field.copy(alias = Some(aliases.next(s"_caliban_gateway_${field.aliasedName}")))
        }
      }
    }
  }

  object Source {
    implicit val ordering: Ordering[Source] = Ordering.by(_.name)
  }

  final case class SourceType(source: Source, typeName: String)
  final case class SourceField(source: String, typeName: String, fieldName: String)

  final case class CostMetadata(
    types: Map[String, BigInt] = Map.empty,
    weights: Map[DirectiveComposition.Coordinate, BigInt] = Map.empty,
    listSizes: Map[SourceField, ListSize] = Map.empty
  )

  final case class ListSize(
    assumedSize: Option[BigInt],
    slicingArguments: List[SlicingArgument],
    sizedFields: List[Vector[String]],
    requireOneSlicingArgument: Boolean
  )

  final case class SlicingArgument(path: ::[String], defaultValue: Option[InputValue], listValued: Boolean)

  final case class KeyField(name: String, children: List[KeyField])

  final case class ContextName(value: String) extends AnyVal

  final case class ContextDeclaration(typeName: String, name: ContextName)

  final case class ContextArgument(at: ArgumentCoordinate, context: ContextName, selections: List[Selection])

  sealed trait OverrideLabel { def value: String }
  object OverrideLabel       {
    final case class Percent(value: String, percentage: BigDecimal) extends OverrideLabel
    final case class Custom(value: String)                          extends OverrideLabel
  }

  // Sources of a field while its progressive override label is active.
  final case class ProgressiveRoute(label: OverrideLabel, sources: List[Source])

  private[internal] final case class SecurityDirectiveApplication(
    source: String,
    typeName: String,
    fieldName: Option[String],
    member: FederationCompilation.FederationDirective.Security,
    groups: List[List[String]]
  ) {
    val coordinate: SchemaCoordinate       =
      fieldName.fold[SchemaCoordinate](SchemaCoordinate.Type(typeName))(SchemaCoordinate.Member(typeName, _))
    val directiveName: String              = s"@${member.name}"
    val scopes: Option[List[List[String]]] =
      if (member == FederationCompilation.FederationDirective.Policy) None else Some(groups)
  }

  private[internal] object SecurityDirectiveApplication {

    def conjunction(expressions: List[List[List[String]]]): List[Set[String]] =
      expressions.foldLeft(List(Set.empty[String])) { (acc, expression) =>
        val normalized = if (expression.isEmpty) List(Nil) else expression
        val combined   = for {
          left  <- acc
          right <- normalized
        } yield left ++ right
        combined.distinct.filterNot(candidate =>
          combined.exists(other => other != candidate && other.subsetOf(candidate))
        )
      }
  }

  final case class EntityLookup(key: List[KeyField], operation: LookupOperation)

  sealed trait LookupOperation

  object LookupOperation {
    case object FederationEntities extends LookupOperation

    sealed trait GraphQLQuery extends LookupOperation {
      def field: String
    }

    final case class Single(field: String, arguments: List[(String, Lookup.Argument[KeyArgument])]) extends GraphQLQuery

    final case class ByKey(field: String, arguments: List[(String, Lookup.Argument[Lookup.Argument[KeyArgument]])])
        extends GraphQLQuery
  }

  final case class KeyArgument(field: String, expectedType: __Type)

}
