package caliban.gateway.internal.composition

import caliban.execution.{ isMetaField, Field }
import caliban.gateway.OperationPolicy.{ SecurityDirective, SecurityRequirement }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.planning.OperationPlan
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.adt.Selection

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
  def hasRequirements: Boolean =
    securityApplications.exists(_.directive != SecurityDirective.UnsupportedPolicy)

  def diagnostics: List[String] =
    securityApplications
      .filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an operation policy."
      )
      .distinct
      .sorted

  def requirements(plan: OperationPlan): List[SecurityRequirement] =
    if (securityApplications.isEmpty) Nil
    else {
      val requested = collectRequirements(plan.fields, root = true)
      // Include implicit fetches and the lookup roots/correlation fields generated later by EntityLookup.
      val injected  =
        if (!hasUnsupportedPolicies) Nil
        else {
          val lookups = plan.entities.flatMap { fetch =>
            val (root, correlation) = fetch.lookup.operation match {
              case LookupOperation.GraphQLQuery(name, _, LookupResult.ByKey(fields)) => name        -> fields.keys.toList
              case LookupOperation.GraphQLQuery(name, _, LookupResult.Single)        => name        -> Nil
              case _: LookupOperation.FederationEntities                             => "_entities" -> Nil
            }
            requirementsAt("Query", None) ::: requirementsAt(
              "Query",
              Some(root)
            ) ::: collectRequirements(
              fieldsForNames(fetch.source, "Query", root :: Nil) :::
                fieldsForNames(fetch.source, fetch.entityType, correlation),
              root = false
            )
          }
          (lookups ::: collectRequirements(
            plan.roots.flatMap(_.downstream) ::: plan.entities.flatMap(_.fields),
            root = false
          ))
            .filter(_.directives.contains(SecurityDirective.UnsupportedPolicy))
        }
      (requested ::: injected).distinct
    }

  private val hasUnsupportedPolicies =
    securityApplications.exists(_.directive == SecurityDirective.UnsupportedPolicy)
  private val dependenciesByField    =
    if (!hasUnsupportedPolicies) Map.empty[TypeField, List[(String, String, List[Selection])]]
    else
      OperationSecurity
        .dependencies(requiredFieldSets, declaredContexts, contextBindings)
        .groupMap(dependency => TypeField(dependency.sourceType, dependency.fieldName))(dependency =>
          (dependency.source, dependency.dependencyType, dependency.selections)
        )
  private val securityDirectives     =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (coordinate, values) => coordinate -> values.map(_.directive).distinct }
  private val securedFieldTypes      = securityApplications
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .groupMap(_._1)(_._2)
    .map { case (fieldName, values) => fieldName -> values.distinct.sorted }
  private val securedTypes           = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted

  private def possibleTypes(typeName: String): Set[String] =
    possibleTypesByName.getOrElse(typeName, Set.empty)

  private def typesOverlap(selectedType: String, candidateType: String, selection: Option[Set[String]]): Boolean =
    selection.getOrElse(possibleTypes(selectedType)).exists(possibleTypes(candidateType))

  private def requirementsAt(typeName: String, fieldName: Option[String]): List[SecurityRequirement] = {
    val values = securityDirectives.getOrElse(typeName -> fieldName, Nil)
    if (values.isEmpty) Nil else SecurityRequirement(typeName, fieldName, values) :: Nil
  }

  // Composition validates these field sets against source definitions, including hidden fields and roots.
  private def fieldsFromSelections(source: String, parent: String, selections: List[Selection]): List[Field] =
    selections.flatMap {
      case field: Selection.Field             =>
        sourceFields.get(SourceField(source, parent, field.name)).toList.map { definition =>
          Field(
            field.name,
            definition._type,
            Some(__Type(kind = __TypeKind.OBJECT, name = Some(parent))),
            fields = fieldsFromSelections(source, definition._type.innerType.name.getOrElse(""), field.selectionSet)
          )
        }
      case fragment: Selection.InlineFragment =>
        fieldsFromSelections(source, fragment.typeCondition.fold(parent)(_.name), fragment.selectionSet)
      case _: Selection.FragmentSpread        => Nil
    }

  private def collectRequirements(
    fields: List[Field],
    root: Boolean,
    visited: Set[TypeField] = Set.empty
  ): List[SecurityRequirement] =
    fields.flatMap { field =>
      if (isMetaField(field)) Nil
      else {
        val parentType       = field.parentType.flatMap(_.innerType.name).getOrElse("")
        val outputType       = field.fieldType.innerType.name.getOrElse("")
        val direct           = requirementsAt(parentType, Some(field.name))
        val rootRequirements = if (root) requirementsAt(parentType, None) else Nil
        val relatedFields    = securedFieldTypes.getOrElse(field.name, Nil).flatMap { typeName =>
          if (typeName != parentType && typesOverlap(parentType, typeName, field._condition))
            requirementsAt(typeName, Some(field.name))
          else Nil
        }
        val output           = requirementsAt(outputType, None)
        val relatedOutput    = securedTypes.flatMap { typeName =>
          if (typeName != outputType && typesOverlap(outputType, typeName, None))
            requirementsAt(typeName, None)
          else Nil
        }
        // Only @policy expands runtime checks to implicit dependencies. Auth/scopes retain their existing
        // client-selection checks and composition-time @requires checks; injected keys add neither.
        val dependencies     =
          if (!hasUnsupportedPolicies) Nil
          else {
            val coordinate = TypeField(parentType, field.name)
            if (visited(coordinate)) Nil
            else
              dependenciesByField.getOrElse(coordinate, Nil).flatMap { case (source, dependencyType, selections) =>
                collectRequirements(
                  fieldsFromSelections(source, dependencyType, selections),
                  root = false,
                  visited + coordinate
                )
                  .filter(_.directives.contains(SecurityDirective.UnsupportedPolicy))
              }
          }
        rootRequirements ::: direct ::: relatedFields ::: output ::: relatedOutput ::: dependencies ::: collectRequirements(
          field.fields,
          root = false,
          visited
        )
      }
    }

  private def fieldsForNames(source: String, parent: String, names: List[String]): List[Field] =
    fieldsFromSelections(source, parent, names.map(name => Selection.Field(None, name, Map.empty, Nil, Nil, 0)))
}

private[composition] object OperationSecurity {
  final case class Dependency(
    source: String,
    sourceType: String,
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
    val required   = requiredFieldSets.toList.map { case (SourceField(source, sourceType, fieldName), selections) =>
      Dependency(source, sourceType, fieldName, sourceType, selections, "@requires")
    }
    val contextual = contextBindings.toList.flatMap { case (SourceField(source, sourceType, fieldName), arguments) =>
      arguments.flatMap { argument =>
        declaredContexts.collect {
          case (SourceType(`source`, contextType), names) if names.contains(argument.context) =>
            Dependency(source, sourceType, fieldName, contextType, argument.selections, "@fromContext")
        }
      }
    }
    required ::: contextual
  }
}
