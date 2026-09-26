package caliban.gateway.internal.planning

import caliban.execution.{ isMetaField, Field }
import caliban.gateway.{ innerParentTypeName, typesOverlap }
import caliban.gateway.PhaseHooks.{ SecurityDirective, SecurityRequirement }
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
      .filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an incoming authorization handler."
      )
      .distinct
      .sorted

  def requirements(plan: OperationPlan): List[SecurityRequirement] =
    if (securityApplications.isEmpty) Nil
    else {
      val fields = plan.fields.filterNot(isMetaField)
      (fields.headOption.toList.flatMap(field => requirementsAt(innerParentTypeName(field), None)) :::
        collectRequirements(fields)).distinct
    }

  private val securityDirectives =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (target, values) => target -> values.map(_.directive).distinct }
  private val securedTypes       = securityApplications
    .groupMap(_.fieldName)(_.typeName)
    .map { case (fieldName, values) => fieldName -> values.distinct.sorted }

  private def requirementsAt(typeName: String, fieldName: Option[String]): List[SecurityRequirement] =
    securityDirectives.get(typeName -> fieldName).map(SecurityRequirement(typeName, fieldName, _)).toList

  private def requirementsFor(
    typeName: String,
    fieldName: Option[String],
    condition: Option[Set[String]]
  ): List[SecurityRequirement] =
    requirementsAt(typeName, fieldName) ::: securedTypes.getOrElse(fieldName, Nil).flatMap { other =>
      if (other != typeName && typesOverlap(possibleTypesByName, typeName, other, condition))
        requirementsAt(other, fieldName)
      else Nil
    }

  private def collectRequirements(fields: List[Field]): List[SecurityRequirement] =
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
