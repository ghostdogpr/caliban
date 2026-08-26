package caliban.gateway.internal

import caliban.InputValue
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Directive, Document, Selection, VariableDefinition }

private[gateway] final class OperationLimits(
  maxTextBytes: Int,
  maxNesting: Int,
  maxNodes: Int
) {

  def textWeight(query: String): Either[OperationLimits.Failure, Int] = {
    val bytes = utf8Length(query)
    if (bytes > maxTextBytes.toLong) Left(OperationLimits.TextTooLarge)
    else if (!OperationLimits.graphQLNestingWithinLimit(query, maxNesting)) Left(OperationLimits.NestingTooDeep)
    else Right(bytes.toInt)
  }

  private def utf8Length(value: String): Long = {
    var bytes = 0L
    var index = 0
    while (index < value.length && bytes <= maxTextBytes.toLong) {
      val current = value.charAt(index)
      if (current <= 0x7f) bytes += 1
      else if (current <= 0x7ff) bytes += 2
      else if (
        Character.isHighSurrogate(current) && index + 1 < value.length &&
        Character.isLowSurrogate(value.charAt(index + 1))
      ) {
        bytes += 4
        index += 1
      } else if (Character.isSurrogate(current)) bytes += 1
      else bytes += 3
      index += 1
    }
    bytes
  }

  def documentWeight(document: Document): Either[OperationLimits.Failure, Int] = {
    var nodes = 0

    def add(): Boolean = {
      nodes += 1
      nodes <= maxNodes
    }

    def input(value: InputValue): Boolean =
      add() && (value match {
        case InputValue.ListValue(values)   => values.forall(input)
        case InputValue.ObjectValue(fields) => fields.values.forall(input)
        case _                              => true
      })

    def directives(values: List[Directive]): Boolean =
      values.forall(value => add() && value.arguments.values.forall(input))

    def variable(value: VariableDefinition): Boolean =
      add() && value.defaultValue.forall(input) && directives(value.directives)

    def selection(value: Selection): Boolean =
      value match {
        case Field(_, _, arguments, values, selections, _) =>
          add() && arguments.values.forall(input) && directives(values) && selections.forall(selection)
        case FragmentSpread(_, values)                     => add() && directives(values)
        case InlineFragment(_, values, selections)         =>
          add() && directives(values) && selections.forall(selection)
      }

    val withinLimit = document.definitions.forall {
      case OperationDefinition(_, _, variables, values, selections) =>
        add() && variables.forall(variable) && directives(values) && selections.forall(selection)
      case FragmentDefinition(_, _, values, selections)             =>
        add() && directives(values) && selections.forall(selection)
      case _                                                        => add()
    }

    if (withinLimit) Right(nodes) else Left(OperationLimits.TooManyNodes)
  }

}

private[gateway] object OperationLimits {
  private val NormalState      = 0
  private val CommentState     = 1
  private val StringState      = 2
  private val BlockStringState = 3

  def graphQLNestingWithinLimit(query: String, maxNesting: Int): Boolean = {
    var index   = 0
    var depth   = 0
    var state   = OperationLimits.NormalState
    var escaped = false

    while (index < query.length) {
      val current = query.charAt(index)
      state match {
        case OperationLimits.NormalState      =>
          if (current == '#') state = OperationLimits.CommentState
          else if (current == '"') {
            if (index + 2 < query.length && query.charAt(index + 1) == '"' && query.charAt(index + 2) == '"') {
              state = OperationLimits.BlockStringState
              index += 2
            } else state = OperationLimits.StringState
          } else if (current == '{' || current == '(' || current == '[') {
            depth += 1
            if (depth > maxNesting) return false
          } else if (current == '}' || current == ')' || current == ']') depth = math.max(0, depth - 1)
        case OperationLimits.CommentState     =>
          if (current == '\n' || current == '\r') state = OperationLimits.NormalState
        case OperationLimits.StringState      =>
          if (escaped) escaped = false
          else if (current == '\\') escaped = true
          else if (current == '"') state = OperationLimits.NormalState
        case OperationLimits.BlockStringState =>
          if (
            current == '\\' && index + 3 < query.length && query.charAt(index + 1) == '"' &&
            query.charAt(index + 2) == '"' && query.charAt(index + 3) == '"'
          ) index += 3
          else if (
            current == '"' && index + 2 < query.length && query.charAt(index + 1) == '"' &&
            query.charAt(index + 2) == '"'
          ) {
            state = OperationLimits.NormalState
            index += 2
          }
      }
      index += 1
    }
    true
  }
  sealed trait Failure
  case object TextTooLarge extends Failure
  case object NestingTooDeep extends Failure
  case object TooManyNodes   extends Failure
}
