package caliban.gateway.internal.planning

import caliban.{ Hash, InputValue }
import caliban.execution.{ isIntrospectionField, isMetaField, Field, Fragment }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.{ PlanExecutionCache, ResponseCompletion }
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
  operationType: OperationType,
  fields: List[Field],
  roots: List[RootFetch],
  entities: List[EntityFetch],
  typenameSelections: List[TypenameSelection],
  passthroughSubgraph: Option[String]
) {

  def render: String = OperationPlan.render(this)

  def rootName: String = OperationPlan.rootName(operationType)

  lazy val localFields: List[Field] = fields.filter(isMetaField)

  lazy val introspectionFields: List[Field] = localFields.filter(isIntrospectionField)

  lazy val hasVariableReferences: Boolean = PlanVariables.hasReferences(this)

  // Cached plans share these artifacts; replacing variable references creates a plan with fresh caches.
  lazy val executionCache: PlanExecutionCache = new PlanExecutionCache
  lazy val completion: ResponseCompletion     = ResponseCompletion.forPlan(this)

  def bind(variables: Map[String, InputValue]): OperationPlan =
    if (hasVariableReferences) PlanVariables.bind(this, variables) else this
}

private[gateway] object OperationPlan {
  final case class FetchId(value: Int) extends AnyVal

  def rootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => "Query"
      case OperationType.Mutation     => "Mutation"
      case OperationType.Subscription => "Subscription"
    }

  final case class RequiredSelection(
    field: String,
    responseName: String,
    children: List[RequiredSelection] = Nil,
    conditions: Option[Set[String]] = None,
    typenameAlias: Option[String] = None
  )

  final case class RootFetch(
    id: FetchId,
    source: String,
    client: List[Field],
    downstream: List[Field]
  )

  final case class ContextualArgument(
    parentType: String,
    field: String,
    argument: String,
    context: ComposedGraph.ContextName,
    sourcePath: Vector[String],
    sourceType: String,
    projection: ContextProjection
  ) {
    def fieldArgument: ComposedGraph.FieldArgument = ComposedGraph.FieldArgument(parentType, field, argument)
  }

  sealed trait ContextProjection {
    def paths: List[List[String]]
  }

  object ContextProjection {
    final case class Path(names: List[String]) extends ContextProjection {
      def paths: List[List[String]] = names :: Nil
    }

    final case class ByType(typenameAlias: String, pathsByType: Map[String, List[String]]) extends ContextProjection {
      def paths: List[List[String]] = pathsByType.values.toList.distinct
    }
  }

  /**
   * The response path and alias of an injected __typename field used during response completion.
   */
  final case class TypenameSelection(path: Vector[String], responseName: String)

  final case class EntityFetch(
    id: FetchId,
    root: FetchId,
    source: String,
    dependencies: Set[FetchId],
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    contextArguments: List[ContextualArgument],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field]
  ) {

    lazy val groupKey: EntityGroupKey =
      EntityGroupKey(source, entityType, lookup, keys, requirements, contextArguments, canonicalSelectionKey(fields))
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

  private[planning] def fieldPaths(fields: List[Field]): List[String] =
    fields.flatMap { field =>
      if (field.fields.isEmpty) List(field.aliasedName)
      else fieldPaths(field.fields).map(child => s"${field.aliasedName}.$child")
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

    val selections = canonicalSelections(fields.map(_.toSelection))
    DocumentRenderer.selectionsRenderer.renderCompact(selections)
  }

  private object PlanVariables {

    def hasReferences(plan: OperationPlan): Boolean =
      plan.fields.exists(fieldReferences) ||
        plan.roots.exists(rootReferences) ||
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
        if (rootReferences(fetch))
          fetch.copy(
            client = fetch.client.map(bindField),
            downstream = fetch.downstream.map(bindField)
          )
        else fetch

      def bindEntity(fetch: EntityFetch): EntityFetch =
        if (fetch.fields.exists(fieldReferences)) fetch.copy(fields = fetch.fields.map(bindField))
        else fetch

      plan.copy(
        fields = plan.fields.map(bindField),
        roots = plan.roots.map(bindRoot),
        entities = plan.entities.map(bindEntity)
      )
    }

    private def rootReferences(fetch: RootFetch): Boolean =
      fetch.client.exists(fieldReferences) || fetch.downstream.exists(fieldReferences)

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
    val rootLines   = plan.roots.flatMap { fetch =>
      fetch.client.zip(fetch.downstream).map { case (client, downstream) =>
        val keySelections = plan.entities
          .find(_.mergePath.headOption.contains(client.aliasedName))
          .toList
          .flatMap(entity => entity.keys ::: entity.typename.toList)
        val fields        = fieldPaths(downstream.fields).map { path =>
          keySelections.find(_.responseName == path).map(selection => s"${selection.field} (key)").getOrElse(path)
        }
        s"fetch ${fetch.source} at $$.${client.aliasedName} fields ${fields.mkString("[", ", ", "]")}"
      }
    }
    val sources     = (plan.roots.iterator.map(fetch => fetch.id -> fetch.source) ++
      plan.entities.iterator.map(fetch => fetch.id -> fetch.source)).toMap
    val entityLines = plan.entities.map { fetch =>
      val dependencies = fetch.dependencies.toList.sortBy(_.value).flatMap(sources.get).distinct.mkString(",")
      s"fetch ${fetch.source} after $dependencies at $$.${fetch.mergePath.mkString(".")} " +
        s"via ${fetch.entityType}(${fetch.keys.map(_.field).mkString(",")}) fields ${fieldPaths(fetch.fields).mkString("[", ", ", "]")}"
    }
    (header :: rootLines ::: entityLines).mkString("\n")
  }

}
