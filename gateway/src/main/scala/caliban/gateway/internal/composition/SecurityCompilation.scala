package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.Value.StringValue
import caliban.gateway._
import caliban.gateway.PhaseHooks.SecurityDirective
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.FederationCompilation.{
  FederationDirectiveNames,
  TypeSystemDirectiveApplication
}
import caliban.introspection.adt.__Field
import caliban.parsing.adt.{ Directive, Selection }
import caliban.schema.RootType

private[composition] object SecurityCompilation {

  def compile(
    source: String,
    names: FederationDirectiveNames,
    applications: List[TypeSystemDirectiveApplication]
  ): Either[List[String], List[SecurityDirectiveApplication]] =
    validateAll(applications.flatMap { application =>
      application.securityCoordinate.toList.flatMap { case (typeName, fieldName) =>
        application.directives.flatMap { directive =>
          compileDirective(source, application.coordinate.display, directive, names).map(_.map {
            case (securityDirective, policies) =>
              SecurityDirectiveApplication(source, typeName, fieldName, securityDirective, policies)
          })
        }
      }
    })

  def diagnostics(
    applications: List[SecurityDirectiveApplication],
    sourceFields: Map[SourceField, __Field],
    requiredFieldSets: Map[SourceField, List[Selection]],
    declaredContexts: Map[SourceType, Set[ContextName]],
    contextBindings: Map[SourceField, List[ContextArgument]],
    possibleTypesByName: Map[String, Set[String]],
    rootType: RootType
  ): List[String] =
    hiddenDiagnostics(applications.filterNot(_.directive == SecurityDirective.UnsupportedPolicy), rootType) :::
      missingTransitiveDiagnostics(
        dependencies(requiredFieldSets, declaredContexts, contextBindings),
        applications,
        sourceFields,
        possibleTypesByName
      )

  private def compileDirective(
    source: String,
    coordinate: String,
    directive: Directive,
    names: FederationDirectiveNames
  ): Option[Either[String, (SecurityDirective, List[List[String]])]] = {
    def invalid(name: String) = s"[$source] Invalid Federation $name application at '$coordinate'."

    if (names.authenticated.contains(directive.name))
      Some(
        if (directive.arguments.isEmpty) Right(SecurityDirective.Authenticated -> Nil)
        else Left(invalid("@authenticated"))
      )
    else if (names.requiresScopes.contains(directive.name))
      Some(
        requirementGroups(directive.arguments, "scopes")
          .map(SecurityDirective.RequiresScopes(_) -> Nil)
          .toRight(invalid("@requiresScopes"))
      )
    else if (names.policy.contains(directive.name))
      Some(
        requirementGroups(directive.arguments, "policies")
          .map(SecurityDirective.UnsupportedPolicy -> _)
          .toRight(invalid("@policy"))
      )
    else None
  }

  private def requirementGroups(arguments: Map[String, InputValue], name: String): Option[List[List[String]]] =
    arguments.get(name) match {
      case Some(InputValue.ListValue(groups)) if arguments.size == 1 =>
        traverseOption(groups) {
          case InputValue.ListValue(values) => stringValues(values)
          case group: StringValue           => Some(List(group.value))
          case _                            => None
        }
      case Some(group: StringValue) if arguments.size == 1           => Some(List(List(group.value)))
      case _                                                         => None
    }

  /**
   * Selections needed to resolve a field. The dependency type is its parent for @requires,
   * or the declaring context's type for @fromContext.
   */
  private final case class Dependency(
    source: String,
    parentType: String,
    fieldName: String,
    dependencyType: String,
    selections: List[Selection],
    directive: String
  )

  private def dependencies(
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

  private def hiddenDiagnostics(
    applications: List[SecurityDirectiveApplication],
    rootType: RootType
  ): List[String] = {
    def isVisible(application: SecurityDirectiveApplication): Boolean =
      rootType.types.get(application.typeName).exists { tpe =>
        application.fieldName.forall(fieldDefinition(tpe, _).nonEmpty)
      }

    applications.filterNot(isVisible).map { application =>
      s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' cannot be enforced because the coordinate is not client-visible."
    }
  }

  private def missingTransitiveDiagnostics(
    dependencies: List[Dependency],
    applications: List[SecurityDirectiveApplication],
    sourceFields: Map[SourceField, __Field],
    possibleTypesByName: Map[String, Set[String]]
  ): List[String] = {
    def applicable(selectedType: String, candidateType: String): Boolean =
      selectedType == candidateType || typesOverlap(possibleTypesByName, selectedType, candidateType, None)

    def typeApplications(typeName: String): List[SecurityDirectiveApplication] =
      applications.filter(application => application.fieldName.isEmpty && applicable(typeName, application.typeName))

    def fieldApplications(typeName: String, fieldName: String): List[SecurityDirectiveApplication] =
      applications.filter(application =>
        application.fieldName.contains(fieldName) && applicable(typeName, application.typeName)
      )

    // Field sets can select fields hidden from clients, so they resolve against the source schema.
    def requiredProfiles(
      source: String,
      selections: List[Selection],
      parentType: String
    ): List[(String, SecurityProfile)] =
      selections.flatMap {
        case field: Selection.Field             =>
          sourceFields.get(SourceField(source, parentType, field.name)).toList.flatMap { definition =>
            val outputType = definition._type.innerType.name
            val required   = SecurityProfile(
              fieldApplications(parentType, field.name) ::: outputType.toList.flatMap(typeApplications)
            )
            (s"$parentType.${field.name}" -> required) ::
              outputType.toList.flatMap(requiredProfiles(source, field.selectionSet, _))
          }
        case fragment: Selection.InlineFragment =>
          val selectedType = fragment.typeCondition.fold(parentType)(_.name)
          (selectedType -> SecurityProfile(typeApplications(selectedType))) ::
            requiredProfiles(source, fragment.selectionSet, selectedType)
        case _: Selection.FragmentSpread        => Nil
      }

    dependencies.flatMap { dependency =>
      val available = SecurityProfile(
        typeApplications(dependency.parentType) ::: fieldApplications(dependency.parentType, dependency.fieldName)
      )
      requiredProfiles(dependency.source, dependency.selections, dependency.dependencyType).collect {
        case (coordinate, required) if !available.implies(required) =>
          s"[${dependency.source}] Field '${dependency.parentType}.${dependency.fieldName}' does not specify sufficient Federation security requirements for ${dependency.directive} dependency '$coordinate'."
      }
    }.distinct.sorted
  }

  private final case class SecurityProfile(
    authenticated: Boolean,
    scopes: Option[List[Set[String]]],
    policies: Option[List[Set[String]]]
  ) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes) &&
        SecurityProfile.implies(policies, required.policies)
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
        scopes,
        conjunction(applications.filter(_.directive == SecurityDirective.UnsupportedPolicy).map(_.policies))
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
