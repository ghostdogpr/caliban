package caliban.gateway.internal.planning

import caliban.{ Hash, InputValue }
import caliban.execution.{ isIntrospectionField, Field, Fragment }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.planning.OperationPlan._
import caliban.parsing.adt.{ Directive, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.Scala3Annotations.threadUnsafe
import caliban.Value.NullValue

import scala.collection.immutable.ListMap

/**
 * The shared contract between planning and execution.
 * Fetch dependencies identify prerequisites; merge paths use response names and omit list indices.
 */
private[gateway] final case class OperationPlan(
  operation: OperationType,
  rootName: String,
  fields: List[Field],
  localFields: List[Field],
  roots: List[RootFetch],
  entities: List[EntityFetch],
  typenameSelections: List[TypenameSelection],
  passthroughSubgraph: Option[String]
) {

  def render: String = OperationPlan.render(this)

  lazy val introspectionFields: List[Field] = localFields.filter(isIntrospectionField)

  lazy val hasVariableReferences: Boolean = PlanVariables.references(this)

  private[internal] def bind(variables: Map[String, InputValue]): OperationPlan =
    PlanVariables.bind(this, variables)
}

private[gateway] object OperationPlan {
  final case class FetchId(value: Int) extends AnyVal

  final case class RequiredSelection(
    field: String,
    responseName: String,
    children: List[RequiredSelection] = Nil,
    conditions: Option[Set[String]] = None,
    runtimeTypeAlias: Option[String] = None
  )

  def privateAlias(base: String, used: Set[String]): String = {
    var candidate = base
    var suffix    = 2
    while (used.contains(candidate)) {
      candidate = s"${base}_$suffix"
      suffix += 1
    }
    candidate
  }

  final case class RootFetch(
    id: FetchId,
    source: String,
    client: List[Field],
    downstream: List[Field],
    contextRoots: List[Field]
  ) {
    def selections: List[Field] =
      if (contextRoots.isEmpty) downstream else downstream ::: contextRoots
  }

  final case class ContextualArgument(
    parentType: String,
    field: String,
    argument: String,
    context: ComposedGraph.ContextName,
    sourcePath: Vector[String],
    sourceType: String,
    selections: List[RequiredSelection],
    typename: Option[RequiredSelection]
  )

  /**
   * The response path and alias of an injected __typename field used during response completion.
   */
  final case class TypenameSelection(path: Vector[String], responseName: String)

  final case class EntityFetch(
    id: FetchId,
    root: FetchId,
    source: String,
    dependencies: Set[FetchId],
    dependencySource: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    contextArguments: List[ContextualArgument],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field],
    mayNeedPrerequisiteFetches: Boolean
  ) {

    lazy val selectionKey: String = canonicalSelectionKey(fields)
  }

  private[internal] def canonicalSelectionKey(fields: List[Field]): String = {
    def renderSelection(selection: Selection): String =
      DocumentRenderer.selectionsRenderer.renderCompact(selection :: Nil)

    def canonicalDirective(directive: Directive): Directive =
      directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*), index = 0)

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
            arguments = ListMap(field.arguments.toList.sortBy(_._1): _*),
            directives = field.directives.map(canonicalDirective),
            selectionSet = canonicalSelections(field.selectionSet),
            index = 0
          )
        case fragment: Selection.InlineFragment =>
          fragment.copy(
            dirs = fragment.dirs.map(canonicalDirective),
            selectionSet = canonicalSelections(fragment.selectionSet)
          )
        case fragment: Selection.FragmentSpread =>
          fragment.copy(directives = fragment.directives.map(canonicalDirective))
      }

    val selections = canonicalSelections(fields.map(_.toSelection))
    DocumentRenderer.selectionsRenderer.renderCompact(selections)
  }

  private[internal] object PlanVariables {

    def references(plan: OperationPlan): Boolean =
      plan.fields.exists(fieldReferences) ||
        plan.roots.exists(fetch => fetch.client.exists(fieldReferences) || fetch.selections.exists(fieldReferences)) ||
        plan.entities.exists(fetch => fetch.fields.exists(fieldReferences))

    def bind(plan: OperationPlan, variables: Map[String, InputValue]): OperationPlan = {
      def bindValue(value: InputValue): Option[InputValue] =
        value match {
          case variable: InputValue.VariableValue =>
            variables.get(variable.name)
          case InputValue.ListValue(values)       =>
            Some(InputValue.ListValue(values.map(value => bindValue(value).getOrElse(NullValue))))
          case InputValue.ObjectValue(fields)     =>
            Some(InputValue.ObjectValue(fields.flatMap { case (name, value) => bindValue(value).map(name -> _) }))
          case value                              => Some(value)
        }

      def bindDirective(directive: Directive): Directive =
        directive.copy(arguments = directive.arguments.flatMap { case (name, value) =>
          bindValue(value).map(name -> _)
        })

      def bindFragment(fragment: Fragment): Fragment =
        fragment.copy(directives = fragment.directives.map(bindDirective))

      def bindField(field: Field): Field =
        field.copy(
          fields = field.fields.map(bindField),
          arguments = field.arguments.flatMap { case (name, value) => bindValue(value).map(name -> _) },
          directives = field.directives.map(bindDirective),
          fragment = field.fragment.map(bindFragment)
        )

      def bindRoot(fetch: RootFetch): RootFetch =
        if (fetch.client.exists(fieldReferences) || fetch.selections.exists(fieldReferences))
          fetch.copy(
            client = fetch.client.map(bindField),
            downstream = fetch.downstream.map(bindField),
            contextRoots = fetch.contextRoots.map(bindField)
          )
        else fetch

      def bindEntity(fetch: EntityFetch): EntityFetch =
        if (fetch.fields.exists(fieldReferences)) fetch.copy(fields = fetch.fields.map(bindField))
        else fetch

      plan.copy(
        fields = plan.fields.map(bindField),
        localFields = plan.localFields.map(bindField),
        roots = plan.roots.map(bindRoot),
        entities = plan.entities.map(bindEntity)
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

  private[internal] final case class EntityGroupKey(
    source: String,
    entityType: String,
    lookup: ComposedGraph.EntityLookup,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    contextArguments: List[ContextualArgument],
    selection: String
  ) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = Hash.caseClassHash(this)
  }

  private[gateway] def logicalCallCount(fetches: List[EntityFetch]): Int = {
    val fetchIds = fetches.iterator.map(_.id).toSet

    def count(
      pending: List[EntityFetch],
      completed: Set[FetchId],
      calls: Int
    ): Int =
      if (pending.isEmpty) calls
      else {
        val ready = pending.filter(fetch => fetch.dependencies.forall(id => completed.contains(id) || !fetchIds(id)))
        if (ready.isEmpty) calls
        else {
          val readyIds = ready.iterator.map(_.id).toSet
          count(
            pending.filterNot(fetch => readyIds.contains(fetch.id)),
            completed ++ readyIds,
            calls + ready.iterator.map(entityGroupKey).toSet.size
          )
        }
      }

    count(fetches, Set.empty, 0)
  }

  private[internal] def entityGroupKey(fetch: EntityFetch): EntityGroupKey =
    EntityGroupKey(
      fetch.source,
      fetch.entityType,
      fetch.lookup,
      fetch.keys,
      fetch.requirements,
      fetch.contextArguments,
      fetch.selectionKey
    )

  private def render(plan: OperationPlan): String = {
    val header = plan.operation.toString.toLowerCase
    val roots  = plan.roots.flatMap { fetch =>
      fetch.client.zip(fetch.downstream).map { case (client, downstream) =>
        val entity = plan.entities.find(_.mergePath.headOption.contains(client.aliasedName))
        val fields = flatten(downstream.fields).map { path =>
          entity
            .flatMap(join =>
              join.keys.find(_.responseName == path).orElse(join.typename.filter(_.responseName == path))
            )
            .map(selection => s"${selection.field} (key)")
            .getOrElse(path)
        }
        s"fetch ${fetch.source} at $$.${client.aliasedName} fields ${fields.mkString("[", ", ", "]")}"
      }
    }
    val joins  = plan.entities.map(fetch =>
      s"fetch ${fetch.source} after ${fetch.dependencySource} at $$.${fetch.mergePath.mkString(".")} " +
        s"via ${fetch.entityType}(${fetch.keys.map(_.field).mkString(",")}) fields ${flatten(fetch.fields).mkString("[", ", ", "]")}"
    )
    (header :: roots ::: joins).mkString("\n")
  }

  private[planning] def flatten(fields: List[Field]): List[String] =
    fields.flatMap { field =>
      if (field.fields.isEmpty) List(field.aliasedName)
      else flatten(field.fields).map(child => s"${field.aliasedName}.$child")
    }

}
