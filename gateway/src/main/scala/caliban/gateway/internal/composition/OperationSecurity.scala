package caliban.gateway.internal.composition

import caliban.gateway.{ innerParentTypeName, EntitiesField }
import caliban.execution.{ isMetaField, Field }
import caliban.gateway.PhaseHooks.{ SecurityDirective, SecurityRequirement }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan.EntityFetch
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.adt.Selection
import caliban.schema.RootType

import scala.collection.compat._

/**
 * Collects security requirements from client selections and implicit fetch dependencies.
 */
private[composition] final class OperationSecurity(
  possibleTypesByName: Map[String, Set[String]],
  sourceFields: Map[SourceField, __Field],
  requiredFieldSets: Map[SourceField, List[Selection]],
  declaredContexts: Map[SourceType, Set[ContextName]],
  contextBindings: Map[SourceField, List[ContextArgument]],
  securityApplications: List[SecurityDirectiveApplication]
) {
  import OperationSecurity.Dependency

  def hasRequirements: Boolean =
    securityApplications.exists(_.directive != SecurityDirective.UnsupportedPolicy)

  def diagnostics: List[String] =
    securityApplications
      .filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an incoming authorization handler."
      )
      .distinct
      .sorted

  def requirements(plan: OperationPlan): List[SecurityRequirement] =
    if (securityApplications.isEmpty) Nil
    else {
      val requested = collectRequirements(plan.fields, atRoot = true)
      (requested ::: injectedRequirements(plan)).distinct
    }

  private val hasUnsupportedPolicies =
    securityApplications.exists(_.directive == SecurityDirective.UnsupportedPolicy)
  private val dependenciesByField    =
    if (!hasUnsupportedPolicies) Map.empty[TypeField, List[Dependency]]
    else
      // A composed field must account for dependencies declared by every source.
      OperationSecurity
        .dependencies(requiredFieldSets, declaredContexts, contextBindings)
        .groupBy(dependency => TypeField(dependency.parentType, dependency.fieldName))
  private val securityDirectives     =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (target, values) => target -> values.map(_.directive).distinct }
  private val typesBySecuredField    = securityApplications
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .groupMap(_._1)(_._2)
    .map { case (fieldName, values) => fieldName -> values.distinct.sorted }
  private val securedTypes           = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted

  // Only @policy applies to injected fetches and lookup roots/correlation fields added by EntityLookup.
  private def injectedRequirements(plan: OperationPlan): List[SecurityRequirement] =
    if (!hasUnsupportedPolicies) Nil
    else {
      val lookups = plan.entities.flatMap(lookupRequirements)
      val fields  = plan.roots.flatMap(_.downstream) ::: plan.entities.flatMap(_.fields)
      policyRequirements(lookups ::: collectRequirements(fields, atRoot = false))
    }

  private def lookupRequirements(fetch: EntityFetch): List[SecurityRequirement] = {
    val (lookupField, correlationFields) = fetch.lookup.operation match {
      case LookupOperation.GraphQLQuery(name, _, LookupResult.ByKey(fields)) => name          -> fields.keys.toList
      case LookupOperation.GraphQLQuery(name, _, LookupResult.Single)        => name          -> Nil
      case _: LookupOperation.FederationEntities                             => EntitiesField -> Nil
    }
    val fields                           = fieldsForNames(fetch.source, "Query", lookupField :: Nil) :::
      fieldsForNames(fetch.source, fetch.entityType, correlationFields)
    requirementsAt("Query", None) ::: requirementsAt("Query", Some(lookupField)) :::
      collectRequirements(fields, atRoot = false)
  }

  private def requirementsAt(typeName: String, fieldName: Option[String]): List[SecurityRequirement] = {
    val values = securityDirectives.getOrElse(typeName -> fieldName, Nil)
    if (values.isEmpty) Nil else SecurityRequirement(typeName, fieldName, values) :: Nil
  }

  // Composition validates these field sets against source definitions, including hidden fields and roots.
  private def fieldsFromSelections(source: String, parent: String, selections: List[Selection]): List[Field] =
    selections.flatMap {
      case field: Selection.Field             =>
        sourceFields
          .get(SourceField(source, parent, field.name))
          .map { definition =>
            Field(
              field.name,
              definition._type,
              Some(__Type(kind = __TypeKind.OBJECT, name = Some(parent))),
              fields = fieldsFromSelections(source, definition._type.innerType.name.getOrElse(""), field.selectionSet)
            )
          }
          .toList
      case fragment: Selection.InlineFragment =>
        fieldsFromSelections(source, fragment.typeCondition.fold(parent)(_.name), fragment.selectionSet)
      case _: Selection.FragmentSpread        => Nil
    }

  private def collectRequirements(
    fields: List[Field],
    atRoot: Boolean,
    visited: Set[TypeField] = Set.empty
  ): List[SecurityRequirement] =
    fields.flatMap { field =>
      if (isMetaField(field)) Nil
      else {
        val parentType       = innerParentTypeName(field)
        val outputType       = field.fieldType.innerType.name.getOrElse("")
        val direct           = requirementsAt(parentType, Some(field.name))
        val rootRequirements = if (atRoot) requirementsAt(parentType, None) else Nil
        val relatedFields    = typesBySecuredField.getOrElse(field.name, Nil).flatMap { typeName =>
          if (
            typeName != parentType && OperationSecurity
              .typesOverlap(possibleTypesByName, parentType, typeName, field._condition)
          )
            requirementsAt(typeName, Some(field.name))
          else Nil
        }
        val output           = requirementsAt(outputType, None)
        val relatedOutput    = securedTypes.flatMap { typeName =>
          if (typeName != outputType && OperationSecurity.typesOverlap(possibleTypesByName, outputType, typeName, None))
            requirementsAt(typeName, None)
          else Nil
        }
        // Only @policy expands runtime checks to implicit dependencies. Auth/scopes retain their existing
        // client-selection checks and composition-time @requires checks; injected keys add neither.
        val dependencies     =
          if (hasUnsupportedPolicies) dependencyRequirements(TypeField(parentType, field.name), visited) else Nil
        val nested           = collectRequirements(field.fields, atRoot = false, visited)
        rootRequirements ::: direct ::: relatedFields ::: output ::: relatedOutput ::: dependencies ::: nested
      }
    }

  private def dependencyRequirements(fieldKey: TypeField, visited: Set[TypeField]): List[SecurityRequirement] =
    // Break dependency cycles along this path without suppressing sibling selections.
    if (visited(fieldKey)) Nil
    else
      dependenciesByField.getOrElse(fieldKey, Nil).flatMap { dependency =>
        val fields = fieldsFromSelections(dependency.source, dependency.dependencyType, dependency.selections)
        policyRequirements(collectRequirements(fields, atRoot = false, visited + fieldKey))
      }

  private def policyRequirements(requirements: List[SecurityRequirement]): List[SecurityRequirement] =
    requirements.filter(_.directives.contains(SecurityDirective.UnsupportedPolicy))

  private def fieldsForNames(source: String, parent: String, names: List[String]): List[Field] =
    fieldsFromSelections(source, parent, names.map(name => Selection.Field(None, name, Map.empty, Nil, Nil, 0)))
}

private[composition] object OperationSecurity {

  /**
   * Selections needed to resolve a field. The dependency type is its parent for @requires,
   * or the declaring context's type for @fromContext.
   */
  final case class Dependency(
    source: String,
    parentType: String,
    fieldName: String,
    dependencyType: String,
    selections: List[Selection],
    directive: String
  )

  def dependencies(
    requiredFieldSets: Map[SourceField, List[Selection]],
    declaredContexts: Map[SourceType, Set[ContextName]],
    contextBindings: Map[SourceField, List[ContextArgument]]
  ): List[Dependency] = {
    val required   = requiredFieldSets.toList.map { case (SourceField(source, parentType, fieldName), selections) =>
      Dependency(source, parentType, fieldName, parentType, selections, "@requires")
    }
    val contextual = contextBindings.toList.flatMap { case (SourceField(source, parentType, fieldName), arguments) =>
      arguments.flatMap { argument =>
        declaredContexts.collect {
          case (SourceType(`source`, contextType), names) if names.contains(argument.context) =>
            Dependency(source, parentType, fieldName, contextType, argument.selections, "@fromContext")
        }
      }
    }
    required ::: contextual
  }

  def hiddenDiagnostics(
    applications: List[SecurityDirectiveApplication],
    rootType: RootType
  ): List[String] = {
    def isVisible(application: SecurityDirectiveApplication): Boolean =
      rootType.types.get(application.typeName).exists { tpe =>
        application.fieldName.forall(name => tpe.allFields.exists(_.name == name))
      }

    applications.filterNot(isVisible).map { application =>
      s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' cannot be enforced because the coordinate is not client-visible."
    }
  }

  def missingTransitiveDiagnostics(
    dependencies: List[OperationSecurity.Dependency],
    applications: List[SecurityDirectiveApplication],
    possibleTypesByName: Map[String, Set[String]],
    rootType: RootType
  ): List[String] = {
    def applicable(selectedType: String, candidateType: String): Boolean =
      selectedType == candidateType || typesOverlap(possibleTypesByName, selectedType, candidateType, None)

    def typeApplications(typeName: String): List[SecurityDirectiveApplication] =
      applications.filter(application => application.fieldName.isEmpty && applicable(typeName, application.typeName))

    def fieldApplications(typeName: String, fieldName: String): List[SecurityDirectiveApplication] =
      applications.filter(application =>
        application.fieldName.contains(fieldName) && applicable(typeName, application.typeName)
      )

    def requiredProfiles(selections: List[Selection], parentType: String): List[(String, SecurityProfile)] =
      selections.flatMap {
        case field: Selection.Field             =>
          rootType.types
            .get(parentType)
            .flatMap(_.allFields.find(_.name == field.name))
            .toList
            .flatMap { definition =>
              val outputType = definition._type.innerType.name
              val required   = SecurityProfile(
                fieldApplications(parentType, field.name) ::: outputType.toList.flatMap(typeApplications)
              )
              (s"$parentType.${field.name}" -> required) ::
                outputType.toList.flatMap(requiredProfiles(field.selectionSet, _))
            }
        case fragment: Selection.InlineFragment =>
          val selectedType = fragment.typeCondition.fold(parentType)(_.name)
          (selectedType -> SecurityProfile(typeApplications(selectedType))) ::
            requiredProfiles(fragment.selectionSet, selectedType)
        case _: Selection.FragmentSpread        => Nil
      }

    dependencies.flatMap { dependency =>
      val available = SecurityProfile(
        typeApplications(dependency.parentType) ::: fieldApplications(dependency.parentType, dependency.fieldName)
      )
      requiredProfiles(dependency.selections, dependency.dependencyType).collect {
        case (coordinate, required) if !available.implies(required) =>
          s"[${dependency.source}] Field '${dependency.parentType}.${dependency.fieldName}' does not specify sufficient Federation security requirements for ${dependency.directive} dependency '$coordinate'."
      }
    }.distinct.sorted
  }

  private def typesOverlap(
    possibleTypesByName: Map[String, Set[String]],
    selectedType: String,
    candidateType: String,
    selection: Option[Set[String]]
  ): Boolean =
    selection
      .getOrElse(possibleTypesByName.getOrElse(selectedType, Set.empty))
      .exists(possibleTypesByName.getOrElse(candidateType, Set.empty))

  private final case class SecurityProfile(authenticated: Boolean, scopes: Option[List[Set[String]]]) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes)
  }

  private object SecurityProfile {
    def apply(applications: List[SecurityDirectiveApplication]): SecurityProfile = {
      val scopes = conjunction(applications.flatMap { application =>
        application.directive match {
          case SecurityDirective.RequiresScopes(values) => Some(values)
          case _                                        => None
        }
      })
      SecurityProfile(
        applications.exists(_.directive == SecurityDirective.Authenticated) || scopes.nonEmpty,
        scopes
      )
    }

    private def conjunction(expressions: List[List[List[String]]]): Option[List[Set[String]]] =
      if (expressions.isEmpty) None
      else
        Some(
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
        )

    private def implies(actual: Option[List[Set[String]]], required: Option[List[Set[String]]]): Boolean = {
      val actualValues   = actual.getOrElse(List(Set.empty[String]))
      val requiredValues = required.getOrElse(List(Set.empty[String]))
      actualValues.forall(value => requiredValues.exists(_.subsetOf(value)))
    }
  }

}
