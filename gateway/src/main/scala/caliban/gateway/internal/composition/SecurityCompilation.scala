package caliban.gateway.internal.composition

import caliban.Value.StringValue
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition.{ FieldCoordinate, TypeCoordinate }
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.parsing.adt.{ Directive, Selection }
import caliban.schema.RootType

private[composition] object SecurityCompilation {

  def compile(subgraph: PreparedSubgraph): List[SecurityDirectiveApplication] =
    subgraph.federationApplications.collect {
      case FederationApplication(TypeCoordinate(typeName, _), member: FederationDirective.Security, directive)      =>
        SecurityDirectiveApplication(subgraph.name, typeName, None, member, groups(directive))
      case FederationApplication(FieldCoordinate(typeName, field), member: FederationDirective.Security, directive) =>
        SecurityDirectiveApplication(subgraph.name, typeName, Some(field), member, groups(directive))
    }

  private def groups(directive: Directive): List[List[String]] =
    directive.arguments.values.toList
      .flatMap(coercedList)
      .map(coercedList(_).collect { case StringValue(value) => value })

  def diagnostics(graph: ComposedGraph): List[String] =
    hiddenDiagnostics(graph.securityApplications.filterNot(_.member == FederationDirective.Policy), graph.rootType) :::
      missingTransitiveDiagnostics(dependencies(graph), graph)

  /**
   * Selections needed to resolve a field. The dependency type is its parent for @requires,
   * or the declaring context's type for @fromContext.
   */
  private final case class Dependency(
    source: Source,
    field: FieldCoordinate,
    dependencyType: String,
    selections: List[Selection],
    member: FederationDirective
  )

  private def dependencies(graph: ComposedGraph): List[Dependency] =
    graph.sources.flatMap { source =>
      source.requiredFieldSets.toList.map { case (field, selections) =>
        Dependency(source, field, field.typeName, selections, FederationDirective.Requires)
      } ::: source.contexts.contextBindings.toList.flatMap { case (field, arguments) =>
        for {
          argument    <- arguments
          declaration <- source.contexts.declarations if declaration.name == argument.context
        } yield Dependency(source, field, declaration.typeName, argument.selections, FederationDirective.FromContext)
      }
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

  private def missingTransitiveDiagnostics(dependencies: List[Dependency], graph: ComposedGraph): List[String] = {
    val applications = graph.securityApplications

    def applicable(selectedType: String, candidateType: String): Boolean =
      selectedType == candidateType || typesOverlap(graph.possibleTypesByName, selectedType, candidateType, None)

    def typeApplications(typeName: String): List[SecurityDirectiveApplication] =
      applications.filter(application => application.fieldName.isEmpty && applicable(typeName, application.typeName))

    def fieldApplications(typeName: String, fieldName: String): List[SecurityDirectiveApplication] =
      applications.filter(application =>
        application.fieldName.contains(fieldName) && applicable(typeName, application.typeName)
      )

    // Field sets can select fields hidden from clients, so they resolve against the source schema.
    def requiredProfiles(
      source: Source,
      selections: List[Selection],
      parentType: String
    ): List[(String, SecurityProfile)] =
      selections.flatMap {
        case field: Selection.Field             =>
          source.sourceField(parentType, field.name).toList.flatMap { definition =>
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

    dependencies.flatMap {
      case Dependency(source, FieldCoordinate(parentType, fieldName), dependencyType, selections, member) =>
        val available = SecurityProfile(typeApplications(parentType) ::: fieldApplications(parentType, fieldName))
        requiredProfiles(source, selections, dependencyType).collect {
          case (coordinate, required) if !available.implies(required) =>
            s"[$source] Field '$parentType.$fieldName' does not specify sufficient Federation security requirements for @${member.name} dependency '$coordinate'."
        }
    }
  }

  private final case class SecurityProfile(
    authenticated: Boolean,
    scopes: List[Set[String]],
    policies: List[Set[String]]
  ) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes) &&
        SecurityProfile.implies(policies, required.policies)
  }

  private object SecurityProfile {
    def apply(applications: List[SecurityDirectiveApplication]): SecurityProfile = {
      def groups(member: FederationDirective.Security) = applications.filter(_.member == member).map(_.groups)
      SecurityProfile(
        applications.exists(application =>
          application.member == FederationDirective.Authenticated || application.member == FederationDirective.RequiresScopes
        ),
        SecurityDirectiveApplication.conjunction(groups(FederationDirective.RequiresScopes)),
        SecurityDirectiveApplication.conjunction(groups(FederationDirective.Policy))
      )
    }

    private def implies(actual: List[Set[String]], required: List[Set[String]]): Boolean =
      actual.forall(value => required.exists(_.subsetOf(value)))
  }

}
