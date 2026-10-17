package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.InputValue.VariableValue
import caliban.Value.{ BooleanValue, StringValue }
import caliban.execution.{ Feature, Field }
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, Directives, Document, Selection }
import caliban.schema.RootType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Incremental delivery with `@defer`. The gateway never forwards the directive: it plans each deferred fragment
 * on its own and sends its fields after the payload that contains its parent.
 */
private[gateway] object Deferral {

  /**
   * A deferred fragment applied to the object at a response path, which excludes list indices.
   */
  final case class DeferPoint(directive: Directive, path: Vector[String]) {
    def label: Option[String] = directive.arguments.get("label").collect { case StringValue(value) => value }
  }

  /**
   * The payload that delivers a field: the innermost deferred fragment comes first, and the initial payload is empty.
   */
  type Delivery = List[DeferPoint]

  def clientRootType(rootType: RootType): RootType =
    rootType.copy(additionalDirectives =
      rootType.additionalDirectives.filterNot(_.name == Directives.Defer) ::: Feature.Defer.directives
    )

  def hasDefer(document: Document, operationName: Option[String]): Boolean =
    document.hasDirective(operationName)(_.name == Directives.Defer)

  /**
   * The delivery of a field selected below an object delivered by `parent` at `path`.
   * Fields without their own deferred fragment, such as fields the gateway injects, share their parent's delivery.
   */
  def delivery(field: Field, parent: Delivery, path: Vector[String]): Delivery =
    field.fragment.fold(parent)(
      _.directives.foldLeft(parent)((delivery, directive) =>
        if (!isActive(directive) || delivery.exists(_.directive == directive)) delivery
        else DeferPoint(directive, path) :: delivery
      )
    )

  /**
   * Wraps every field at the object level of deferred fragments in a fragment carrying their `@defer` directives,
   * outermost first. Each field then records its own deferred fragments, so a field selected both inside and outside
   * them stays in the earlier payload with its whole selection when the selections merge. Validate the client's
   * document, not the result: the copies repeat each label.
   *
   * In a deferred fragment, a fragment spread becomes a spread of a copy of the fragment wrapped this way. Within one
   * selection set, later spreads of the same fragment under the same type condition share that copy, as their fields
   * would merge into the first ones, unless they sit in an outer deferred fragment that would win the merge. Each
   * selection set is rewritten once, so the result grows polynomially with the document, cycles included. Excluded
   * selections stay as they are, so they never decide which copy is shared.
   */
  def pushDown(document: Document, variables: Map[String, InputValue]): Document = {
    val fragments = document.fragmentDefinitions.map(definition => definition.name -> definition).toMap
    val copies    = ListBuffer.empty[FragmentDefinition]
    val rewritten = new java.util.IdentityHashMap[List[Selection], List[Selection]]()
    val ids       = Iterator.from(0)

    def isIncluded(directives: List[Directive]): Boolean =
      directives.forall(directive =>
        if (directive.name == "skip") !ifArgument(directive, variables).getOrElse(false)
        else directive.name != "include" || ifArgument(directive, variables).getOrElse(true)
      )

    def deferred(directives: List[Directive]): List[Directive] = directives.filter(isActive(_, variables))

    def objectLevel(values: List[Selection]): List[Selection] =
      Option(rewritten.get(values)).getOrElse {
        // A cyclic document reaches this selection set again while rewriting it, and leaves it as it is.
        rewritten.put(values, values)
        val shared = mutable.HashMap.empty[(String, Option[NamedType]), (List[Directive], String)]

        def selections(
          values: List[Selection],
          region: List[Directive],
          condition: Option[NamedType]
        ): List[Selection] =
          values.map {
            case field: Selection.Field if isIncluded(field.directives)                             =>
              val pushed = field.copy(selectionSet = objectLevel(field.selectionSet))
              if (region.isEmpty) pushed else Selection.InlineFragment(condition, region, pushed :: Nil)
            case Selection.InlineFragment(typeCondition, directives, set) if isIncluded(directives) =>
              val inner = selections(set, region ::: deferred(directives), typeCondition.orElse(condition))
              Selection.InlineFragment(typeCondition, directives, inner)
            case spread @ Selection.FragmentSpread(name, directives) if isIncluded(directives)      =>
              val inner = region ::: deferred(directives)
              fragments.get(name).filter(_ => inner.nonEmpty).fold[Selection](spread) { definition =>
                Selection.FragmentSpread(copyOf(definition, inner, condition), directives)
              }
            case excluded                                                                           => excluded
          }

        def copyOf(definition: FragmentDefinition, region: List[Directive], condition: Option[NamedType]): String =
          shared.get(definition.name -> condition) match {
            case Some((outer, name)) if !outer.startsWith(region) || outer == region => name
            case _                                                                   =>
              val name     = s"${definition.name}#${ids.next()}"
              shared.update(definition.name -> condition, (region, name))
              val selected = selections(definition.selectionSet, region, Some(definition.typeCondition))
              copies += definition.copy(name = name, selectionSet = selected)
              name
          }

        val result = selections(values, Nil, None)
        rewritten.put(values, result)
        result
      }

    val definitions = document.definitions.map {
      case operation: OperationDefinition => operation.copy(selectionSet = objectLevel(operation.selectionSet))
      case definition: FragmentDefinition => definition.copy(selectionSet = objectLevel(definition.selectionSet))
      case other                          => other
    }
    document.copy(definitions = definitions ::: copies.toList)
  }

  /**
   * The value of the `if` argument of a directive, unless it is missing or an unknown variable.
   */
  private def ifArgument(directive: Directive, variables: Map[String, InputValue]): Option[Boolean] =
    directive.arguments
      .get("if")
      .flatMap {
        case VariableValue(name) => variables.get(name)
        case value               => Some(value)
      }
      .collect { case BooleanValue(value) => value }

  /**
   * A `@defer` that is enabled, or whose condition is a variable resolved later.
   */
  private def isActive(directive: Directive, variables: Map[String, InputValue] = Map.empty): Boolean =
    directive.name == Directives.Defer && ifArgument(directive, variables).getOrElse(true)
}
