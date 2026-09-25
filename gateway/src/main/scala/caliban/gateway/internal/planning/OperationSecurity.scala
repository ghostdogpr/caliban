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
    else collectRequirements(plan.fields, atRoot = true).distinct

  private val securityDirectives  =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (target, values) => target -> values.map(_.directive).distinct }
  private val typesBySecuredField = securityApplications
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .groupMap(_._1)(_._2)
    .map { case (fieldName, values) => fieldName -> values.distinct.sorted }
  private val securedTypes        = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted

  private def requirementsAt(typeName: String, fieldName: Option[String]): List[SecurityRequirement] = {
    val values = securityDirectives.getOrElse(typeName -> fieldName, Nil)
    if (values.isEmpty) Nil else SecurityRequirement(typeName, fieldName, values) :: Nil
  }

  private def collectRequirements(fields: List[Field], atRoot: Boolean): List[SecurityRequirement] =
    fields.flatMap { field =>
      if (isMetaField(field)) Nil
      else {
        val parentType       = innerParentTypeName(field)
        val outputType       = field.fieldType.innerType.name.getOrElse("")
        val direct           = requirementsAt(parentType, Some(field.name))
        val rootRequirements = if (atRoot) requirementsAt(parentType, None) else Nil
        val relatedFields    = typesBySecuredField.getOrElse(field.name, Nil).flatMap { typeName =>
          if (typeName != parentType && typesOverlap(possibleTypesByName, parentType, typeName, field._condition))
            requirementsAt(typeName, Some(field.name))
          else Nil
        }
        val output           = requirementsAt(outputType, None)
        val relatedOutput    = securedTypes.flatMap { typeName =>
          if (typeName != outputType && typesOverlap(possibleTypesByName, outputType, typeName, None))
            requirementsAt(typeName, None)
          else Nil
        }
        val nested           = collectRequirements(field.fields, atRoot = false)
        rootRequirements ::: direct ::: relatedFields ::: output ::: relatedOutput ::: nested
      }
    }
}
