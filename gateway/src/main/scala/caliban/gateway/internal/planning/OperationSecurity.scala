package caliban.gateway.internal.planning

import caliban.execution.{ isMetaField, Field }
import caliban.gateway.{ innerParentTypeName, traverseOption, typesOverlap }
import caliban.gateway.PhaseHooks.SecurityRequirement
import caliban.gateway.internal.composition.ComposedGraph.SecurityDirectiveApplication

import scala.collection.compat._

/**
 * Collects security requirements from client selections.
 */
private[gateway] final class OperationSecurity(
  possibleTypesByName: Map[String, Set[String]],
  securityApplications: List[SecurityDirectiveApplication]
) {

  def diagnostics: List[String] =
    securityApplications
      .filter(_.scopes.nonEmpty)
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an incoming authorization handler."
      )
      .distinct
      .sorted

  // `None` when the operation selects a `@policy` coordinate, which the gateway cannot enforce.
  def requirements(plan: OperationPlan): Option[List[SecurityRequirement]] =
    if (securityApplications.isEmpty) Some(Nil)
    else {
      val fields = plan.fields.filterNot(isMetaField)
      traverseOption(
        (fields.headOption.toList.flatMap(field => requirementsAt(innerParentTypeName(field), None)) :::
          collectRequirements(fields)).distinct
      )(identity)
    }

  private val securityScopes =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (target, values) =>
        target -> traverseOption(values)(_.scopes).map(SecurityDirectiveApplication.conjunction)
      }
  private val securedTypes   = securityApplications
    .groupMap(_.fieldName)(_.typeName)
    .map { case (fieldName, values) => fieldName -> values.distinct.sorted }

  private def requirementsAt(typeName: String, fieldName: Option[String]): List[Option[SecurityRequirement]] =
    securityScopes.get(typeName -> fieldName).map(_.map(SecurityRequirement(typeName, fieldName, _))).toList

  private def requirementsFor(
    typeName: String,
    fieldName: Option[String],
    condition: Option[Set[String]]
  ): List[Option[SecurityRequirement]] =
    requirementsAt(typeName, fieldName) ::: securedTypes.getOrElse(fieldName, Nil).flatMap { other =>
      if (other != typeName && typesOverlap(possibleTypesByName, typeName, other, condition))
        requirementsAt(other, fieldName)
      else Nil
    }

  private def collectRequirements(fields: List[Field]): List[Option[SecurityRequirement]] =
    fields.flatMap { field =>
      if (isMetaField(field)) Nil
      else {
        val parentType = innerParentTypeName(field)
        val outputType = field.fieldType.innerType.name.getOrElse("")
        requirementsFor(parentType, Some(field.name), field._condition) :::
          requirementsFor(outputType, None, None) ::: collectRequirements(field.fields)
      }
    }
}
