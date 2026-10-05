package caliban.gateway.internal.planning

import caliban.{ Hash, InputValue }
import caliban.execution.{ isIntrospectionField, isMetaField, Field, Fragment }
import caliban.gateway.{ responseNames, TypenameField }
import caliban.gateway.internal.composition.{ ComposedGraph, DirectiveComposition, FieldSelectionMap }
import caliban.gateway.internal.execution.{ EntityLookup, PlanExecutor, ResponseCompletion }
import caliban.gateway.internal.planning.OperationPlan._
import caliban.introspection.adt.__Type
import caliban.parsing.adt.{ Directive, OperationType, Selection }
import caliban.parsing.adt.Type.NamedType
import caliban.rendering.DocumentRenderer
import caliban.Scala3Annotations.threadUnsafe
import caliban.Value.NullValue

import scala.collection.immutable.ListMap

/**
 * The shared contract between planning and execution.
 * Fetch dependencies identify prerequisites; merge paths use response names and omit list indices.
 */
private[gateway] final case class OperationPlan(
  operationType: OperationType,
  operationName: Option[String],
  fields: List[Field],
  roots: List[RootFetch],
  entityWaves: List[List[EntityFetch]],
  typenameSelections: List[TypenameSelection],
  passthroughSubgraph: Option[ComposedGraph.Source]
) {

  def render: String = OperationPlan.render(this)

  def rootName: String = ComposedGraph.rootName(operationType)

  lazy val entities: List[EntityFetch] = entityWaves.flatten

  lazy val introspectionFields: List[Field] = fields.filter(isIntrospectionField)

  lazy val hasLocalFields: Boolean = fields.exists(isMetaField)

  lazy val hasVariableReferences: Boolean = PlanVariables.hasReferences(this)

  // Cached plans share these artifacts; replacing variable references creates a plan with fresh caches.
  lazy val preparedRoots: List[PlanExecutor.PreparedRoot] = roots.map(PlanExecutor.prepareRoot(this, _))
  lazy val completion: ResponseCompletion                 = ResponseCompletion.forPlan(this)

  def bind(variables: Map[String, InputValue]): OperationPlan =
    if (hasVariableReferences) PlanVariables.bind(this, variables) else this
}

private[gateway] object OperationPlan {
  final case class FetchId(value: Int) extends AnyVal

  final case class RequiredSelection(
    field: String,
    responseName: String,
    children: List[RequiredSelection] = Nil,
    conditions: Option[Set[String]] = None,
    typenameAlias: Option[String] = None
  )

  final case class RootFetch(id: FetchId, source: ComposedGraph.Source, downstream: List[Field])

  final case class ContextualArgument(
    at: DirectiveComposition.ArgumentCoordinate,
    context: ComposedGraph.ContextName,
    sourcePath: Vector[String],
    selections: List[RequiredSelection],
    typenameAlias: Option[String]
  )

  /**
   * A `@require` argument the gateway fills per entity: its selection map reads the entity's argument requirements
   * under their response names, since it may select one field twice, and its value is coerced to `inputType`.
   */
  final case class RequiredArgument(
    at: DirectiveComposition.ArgumentCoordinate,
    value: FieldSelectionMap.SelectedValue,
    inputType: __Type
  )

  /**
   * The response path and alias of an injected __typename field used during response completion.
   */
  final case class TypenameSelection(path: Vector[String], responseName: String)

  final case class EntityTarget(
    source: ComposedGraph.Source,
    entityType: String,
    lookup: ComposedGraph.EntityLookup,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    argumentRequirements: List[RequiredSelection],
    requiredArguments: List[RequiredArgument],
    contextArguments: List[ContextualArgument]
  ) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = Hash.caseClassHash(this)

    def parentSelections: List[RequiredSelection] = keys ::: requirements ::: argumentRequirements
  }

  final case class EntityFetch(
    id: FetchId,
    root: FetchId,
    dependencies: Set[FetchId],
    mergePath: Vector[String],
    target: EntityTarget,
    typenameAlias: Option[String],
    fields: List[Field]
  ) {

    private[internal] lazy val lookupVariant = EntityLookup.variant(this, Map.empty)

    lazy val groupKey: (EntityTarget, String) = {
      val selections =
        fields.map(field => if (field.targets.contains(Set(target.entityType))) field.copy(targets = None) else field)
      target -> canonicalSelectionKey(selections)
    }
  }

  private[planning] def fieldPaths(fields: List[Field]): List[String] =
    fields.flatMap { field =>
      if (field.fields.isEmpty) List(field.aliasedName)
      else fieldPaths(field.fields).map(child => s"${field.aliasedName}.$child")
    }

  private[internal] def targetedSelections(field: Field): List[Selection] =
    field.targets match {
      case Some(targets) =>
        targets.toList.sorted.map(target =>
          Selection.InlineFragment(Some(NamedType(target, nonNull = false)), Nil, field.toSelection :: Nil)
        )
      case None          => field.toSelection :: Nil
    }

  private def canonicalSelectionKey(fields: List[Field]): String = {
    def renderSelection(selection: Selection): String =
      DocumentRenderer.selectionsRenderer.renderCompact(selection :: Nil)

    def sortedArguments(arguments: Map[String, InputValue]): ListMap[String, InputValue] =
      ListMap(arguments.toList.sortBy(_._1): _*)

    def canonicalSelections(selections: List[Selection]): List[Selection] = {
      val canonical = selections.map(canonicalSelection)
      canonical match {
        case Nil | _ :: Nil => canonical
        case _              =>
          canonical
            .map(selection => renderSelection(selection) -> selection)
            .sortBy(_._1)
            .map(_._2)
      }
    }

    def canonicalSelection(selection: Selection): Selection =
      selection match {
        case field: Selection.Field             =>
          field.copy(
            arguments = sortedArguments(field.arguments),
            directives =
              field.directives.map(directive => directive.copy(arguments = sortedArguments(directive.arguments))),
            selectionSet = canonicalSelections(field.selectionSet)
          )
        case fragment: Selection.InlineFragment =>
          fragment.copy(selectionSet = canonicalSelections(fragment.selectionSet))
        case spread: Selection.FragmentSpread   => spread
      }

    val selections = canonicalSelections(fields.flatMap(targetedSelections))
    DocumentRenderer.selectionsRenderer.renderCompact(selections)
  }

  private object PlanVariables {

    def hasReferences(plan: OperationPlan): Boolean =
      plan.fields.exists(fieldReferences) ||
        plan.roots.exists(_.downstream.exists(fieldReferences)) ||
        plan.entities.exists(fetch => fetch.fields.exists(fieldReferences))

    def bind(plan: OperationPlan, variables: Map[String, InputValue]): OperationPlan = {
      def bindValue(value: InputValue): Option[InputValue] =
        value match {
          case variable: InputValue.VariableValue =>
            variables.get(variable.name)
          case InputValue.ListValue(values)       =>
            Some(InputValue.ListValue(values.map(value => bindValue(value).getOrElse(NullValue))))
          case InputValue.ObjectValue(fields)     =>
            Some(InputValue.ObjectValue(bindArguments(fields)))
          case value                              => Some(value)
        }

      def bindArguments(arguments: Map[String, InputValue]): Map[String, InputValue] =
        arguments.flatMap { case (name, value) => bindValue(value).map(name -> _) }

      def bindDirective(directive: Directive): Directive =
        directive.copy(arguments = bindArguments(directive.arguments))

      def bindFragment(fragment: Fragment): Fragment =
        fragment.copy(directives = fragment.directives.map(bindDirective))

      def bindField(field: Field): Field =
        field.copy(
          fields = field.fields.map(bindField),
          arguments = bindArguments(field.arguments),
          directives = field.directives.map(bindDirective),
          fragment = field.fragment.map(bindFragment)
        )

      def bindRoot(fetch: RootFetch): RootFetch =
        if (fetch.downstream.exists(fieldReferences)) fetch.copy(downstream = fetch.downstream.map(bindField))
        else fetch

      def bindEntity(fetch: EntityFetch): EntityFetch =
        if (fetch.fields.exists(fieldReferences)) fetch.copy(fields = fetch.fields.map(bindField))
        else fetch

      plan.copy(
        fields = plan.fields.map(bindField),
        roots = plan.roots.map(bindRoot),
        entityWaves = plan.entityWaves.map(_.map(bindEntity))
      )
    }

    private def fieldReferences(field: Field): Boolean =
      field.arguments.valuesIterator.exists(valueReferences) ||
        field.directives.exists(directiveReferences) ||
        field.fragment.exists(fragmentReferences) ||
        field.fields.exists(fieldReferences)

    private def fragmentReferences(fragment: Fragment): Boolean =
      fragment.directives.exists(directiveReferences)

    private def directiveReferences(directive: Directive): Boolean =
      directive.arguments.valuesIterator.exists(valueReferences)

    private def valueReferences(value: InputValue): Boolean =
      value match {
        case _: InputValue.VariableValue    => true
        case InputValue.ListValue(values)   => values.exists(valueReferences)
        case InputValue.ObjectValue(fields) => fields.valuesIterator.exists(valueReferences)
        case _                              => false
      }
  }

  private def render(plan: OperationPlan): String = {
    val header      = plan.operationType.toString.toLowerCase
    val clientNames = responseNames(plan.fields)
    val rootLines   = plan.roots.flatMap { fetch =>
      fetch.downstream.filter(field => clientNames(field.aliasedName)).map { downstream =>
        val keySelections = plan.entities
          .find(_.mergePath.headOption.contains(downstream.aliasedName))
          .toList
          .flatMap(entity =>
            entity.target.keys ::: entity.typenameAlias.map(RequiredSelection(TypenameField, _)).toList
          )
        val fields        = fieldPaths(downstream.fields).map { path =>
          keySelections.find(_.responseName == path).map(selection => s"${selection.field} (key)").getOrElse(path)
        }
        s"fetch ${fetch.source} at $$.${downstream.aliasedName} fields ${fields.mkString("[", ", ", "]")}"
      }
    }
    val sources     = (plan.roots.iterator.map(fetch => fetch.id -> fetch.source) ++
      plan.entities.iterator.map(fetch => fetch.id -> fetch.target.source)).toMap
    val entityLines = plan.entities.map { fetch =>
      val dependencies = fetch.dependencies.toList.sortBy(_.value).flatMap(sources.get).distinct.mkString(",")
      s"fetch ${fetch.target.source} after $dependencies at $$.${fetch.mergePath.mkString(".")} " +
        s"via ${fetch.target.entityType}(${fetch.target.keys.map(_.field).mkString(",")}) fields ${fieldPaths(fetch.fields)
            .mkString("[", ", ", "]")}"
    }
    (header :: rootLines ::: entityLines).mkString("\n")
  }

}
