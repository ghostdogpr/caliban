package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.InputValue.{ ListValue, ObjectValue, VariableValue }
import caliban.Value.NullValue
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph.KeyField
import caliban.introspection.adt._
import caliban.parsing.parsers.Parsers.{ name, value, whitespace }
import caliban.rendering.DocumentRenderer
import fastparse._

import scala.annotation.tailrec

// The FieldSelectionMap of GraphQL Federation Appendix A; the shorthand `{ f(args) }` reads as `{ f: f(args) }`.
private[gateway] object FieldSelectionMap {
  import Selection._

  type SelectedValue = ::[Selection]

  sealed trait Selection

  object Selection {
    final case class Leaf(path: ::[PathStep])                                extends Selection
    final case class ObjectOf(path: List[PathStep], fields: ::[ObjectField]) extends Selection
    final case class ListOf(path: List[PathStep], element: SelectedValue)    extends Selection
  }

  final case class PathStep(typeCondition: Option[String], field: String, arguments: Map[String, InputValue])
  final case class ObjectField(name: String, value: SelectedValue)

  def parse(value: String): Either[String, SelectedValue] =
    fastparse.parse(value, selectionMap(_)) match {
      case Parsed.Success(selected, _) => Right(selected)
      case failure: Parsed.Failure     => Left(failure.msg)
    }

  def field(name: String): Selection = Leaf(::(PathStep(None, name, Map.empty), Nil))

  /**
   * Field types resolve through `types`, so a view merged from several schemas can be validated against.
   */
  def validate(
    value: SelectedValue,
    inputType: __Type,
    outputType: __Type,
    types: Map[String, __Type]
  ): List[String] =
    new Validation(types).selected(value, inputType, outputType)

  /**
   * Keeps the alternatives whose root type conditions admit a runtime type, and drops those conditions.
   * `None` when no alternative applies to that type.
   */
  def narrow(selection: Selection, admits: String => Boolean): Option[Selection] = {
    def path(steps: ::[PathStep]): Option[::[PathStep]] =
      if (steps.head.typeCondition.forall(admits)) Some(::(steps.head.copy(typeCondition = None), steps.tail))
      else None

    def narrowed(field: ObjectField): Option[ObjectField] =
      field.value.flatMap(narrow(_, admits)) match {
        case head :: tail => Some(ObjectField(field.name, ::(head, tail)))
        case Nil          => None
      }

    selection match {
      case Leaf(steps)                    => path(steps).map(Leaf(_))
      case ObjectOf(head :: tail, fields) => path(::(head, tail)).map(ObjectOf(_, fields))
      case ObjectOf(Nil, fields)          =>
        traverseOption(fields)(narrowed).collect { case head :: tail => ObjectOf(Nil, ::(head, tail)) }
      case ListOf(head :: tail, element)  => path(::(head, tail)).map(ListOf(_, element))
      case list @ ListOf(Nil, _)          => Some(list)
    }
  }

  /**
   * The fields a selection map reads, merged into one selection; type conditions below the root stay on their fields.
   */
  def keyFields(value: List[Selection]): List[KeyField] = {
    def merged(paths: List[List[PathStep]]): List[KeyField] =
      paths.collect { case step :: _ => (step.field, step.typeCondition) }.distinct.map { case (name, condition) =>
        val rest = paths.collect { case step :: tail if step.field == name && step.typeCondition == condition => tail }
        KeyField(name, merged(rest), condition)
      }

    merged(paths(value))
  }

  def paths(value: List[Selection]): List[List[PathStep]] = value.flatMap {
    case Leaf(steps)             => List(steps)
    case ObjectOf(steps, fields) => fields.flatMap(field => paths(field.value)).map(steps ::: _)
    case ListOf(steps, element)  => paths(element).map(steps ::: _)
  }

  /**
   * Reads a selection map from the values of its key fields. A choice takes its first alternative with a non-null
   * value, or a null one when every alternative is null; a field outside a type condition that did not apply is
   * absent, so its alternative does not resolve.
   */
  def evaluate(selection: Selection, keys: List[(String, InputValue)]): Option[InputValue] =
    evaluate(selection, null, keys)

  /**
   * A null scope is the root, whose fields come from `keys` so no object is built per entity.
   */
  private def evaluate(value: SelectedValue, scope: InputValue, keys: List[(String, InputValue)]): Option[InputValue] =
    if (value.tail.isEmpty) evaluate(value.head, scope, keys)
    else {
      val resolved = value.flatMap(evaluate(_, scope, keys))
      resolved.find(_ != NullValue).orElse(resolved.headOption)
    }

  private def evaluate(selection: Selection, scope: InputValue, keys: List[(String, InputValue)]): Option[InputValue] =
    selection match {
      case Leaf(steps)             => walk(steps, scope, keys)
      case ObjectOf(steps, fields) =>
        walk(steps, scope, keys).flatMap {
          case NullValue => Some(NullValue)
          case target    =>
            traverseOption(fields)(field => evaluate(field.value, target, keys).map(field.name -> _))
              .map(values => ObjectValue(values.toMap))
        }
      case ListOf(steps, element)  =>
        walk(steps, scope, keys).flatMap {
          case NullValue         => Some(NullValue)
          case ListValue(values) => traverseOption(values)(evaluate(element, _, keys)).map(ListValue(_))
          case _                 => None
        }
    }

  @tailrec
  private def walk(steps: List[PathStep], scope: InputValue, keys: List[(String, InputValue)]): Option[InputValue] =
    steps match {
      case Nil          => Some(scope)
      case step :: rest =>
        val next = scope match {
          case null                => keys.collectFirst { case (step.field, value) => value }
          case ObjectValue(fields) => fields.get(step.field)
          case NullValue           => Some(NullValue)
          case _                   => None
        }
        next match {
          case Some(value) if rest.nonEmpty => walk(rest, value, keys)
          case _                            => next
        }
    }

  private def selectionMap(implicit ev: P[Any]): P[SelectedValue] = Start ~ selectedValue ~ End

  private def selectedValue(implicit ev: P[Any]): P[SelectedValue] = nonEmpty("|".? ~ selection, "|" ~ selection)

  private def selection(implicit ev: P[Any]): P[Selection] =
    objectFields.map(ObjectOf(Nil, _)) | path.flatMap(shape(_))

  private def shape(path: ::[PathStep])(implicit ev: P[Any]): P[Selection] =
    ("." ~ objectFields).map(ObjectOf(path, _)) | listElement.map(ListOf(path, _)) | Pass(Leaf(path))

  private def path(implicit ev: P[Any]): P[::[PathStep]] = nonEmpty(step(typeCondition.?), step(link))

  private def typeCondition(implicit ev: P[Any]): P[String] = "<" ~ name ~ ">" ~ "."

  private def link(implicit ev: P[Any]): P[Option[String]] =
    typeCondition.map(Option(_)) | ("." ~ !"{").map(_ => None)

  private def step(condition: => P[Option[String]])(implicit ev: P[Any]): P[PathStep] =
    (condition ~ name ~ arguments).map((PathStep.apply _).tupled)

  private def arguments(implicit ev: P[Any]): P[Map[String, InputValue]] =
    ("(" ~ (name ~ ":" ~ value.filter(isConstant)).rep(1) ~ ")")
      .filter(arguments => arguments.map(_._1).distinct.size == arguments.size)
      .?
      .map(_.fold(Map.empty[String, InputValue])(_.toMap))

  private def isConstant(value: InputValue): Boolean = value match {
    case _: VariableValue    => false
    case ListValue(values)   => values.forall(isConstant)
    case ObjectValue(fields) => fields.values.forall(isConstant)
    case _                   => true
  }

  private def objectFields(implicit ev: P[Any]): P[::[ObjectField]] = "{" ~ nonEmpty(objectField, objectField) ~ "}"

  private def objectField(implicit ev: P[Any]): P[ObjectField] =
    (name ~ ":" ~ selectedValue).map { case (field, value) => ObjectField(field, value) } |
      step(Pass(Option.empty[String])).map(step => ObjectField(step.field, ::(Leaf(::(step, Nil)), Nil)))

  private def nonEmpty[A](first: => P[A], rest: => P[A])(implicit ev: P[Any]): P[::[A]] =
    (first ~ rest.rep).map { case (head, tail) => ::(head, tail.toList) }

  private def listElement(implicit ev: P[Any]): P[SelectedValue] =
    "[" ~ (listElement.map(nested => ::[Selection](ListOf(Nil, nested), Nil)) | selectedValue) ~ "]"

  private final class Validation(types: Map[String, __Type]) {
    def selected(value: SelectedValue, input: __Type, output: __Type): List[String] =
      value.flatMap {
        case Leaf(path)             => walk(path, output).fold(identity, leaf(_, input))
        case ObjectOf(path, fields) => walk(path, output).fold(identity, objectSelection(fields, input, _))
        case ListOf(path, element)  => walk(path, output).fold(identity, list(element, input, _))
      }

    private def walk(path: List[PathStep], scope: __Type): Either[List[String], __Type] =
      path.foldLeft[Either[List[String], __Type]](Right(scope))((current, step) => current.flatMap(select(_, step)))

    private def select(scope: __Type, step: PathStep): Either[List[String], __Type] =
      for {
        parent <- composite(scope, step.field)
        narrow <- step.typeCondition.fold[Either[List[String], __Type]](Right(parent))(narrowed(parent, _))
        field  <- fieldDefinition(narrow, step.field).toRight(
                    List(s"Field '${step.field}' is not defined on type '${render(narrow)}'.")
                  )
        errors  = argumentErrors(s"field '${render(narrow)}.${field.name}'", field.allArgs, step.arguments, exempt)
        tpe    <- validated(errors, Right(TypeComposition.rewriteType(field._type, types)))
      } yield tpe

    private def composite(scope: __Type, field: String): Either[List[String], __Type] = {
      val parent = nullableType(scope)
      if (isCompositeType(parent)) Right(parent)
      else if (!isCompositeType(scope.innerType))
        Left(List(s"Field '$field' cannot be selected on leaf type '${render(scope)}'."))
      else Left(List(s"Field '$field' cannot be selected on list type '${render(scope)}'."))
    }

    private def narrowed(parent: __Type, condition: String): Either[List[String], __Type] =
      types
        .get(condition)
        .filter(tpe => parent.possibleTypeNames.exists(tpe.possibleTypeNames))
        .toRight(List(s"Type '$condition' is not a possible type of '${render(parent)}'."))

    private val exempt: __InputValue => Boolean = _.directives.getOrElse(Nil).exists(_.name == "require")

    private def leaf(selected: __Type, input: __Type): List[String] =
      if (isCompositeType(selected.innerType)) List(s"Selected type '${render(selected)}' must have subselections.")
      else
        check(
          compatibleValueType(selected, input),
          s"Selected type '${render(selected)}' does not match input type '${render(input)}'."
        )

    private def objectSelection(fields: ::[ObjectField], input: __Type, scope: __Type): List[String] = {
      val inputObject           = nullableType(input)
      val names                 = fields.map(_.name)
      val repeated              = duplicates(names).sorted.map(name => s"Input field '$name' is selected more than once.")
      def missing: List[String] =
        if (inputObject._isOneOfInput)
          check(names.distinct.size == 1, s"Selection on oneOf input type '${render(input)}' must select one field.")
        else
          inputObject.allInputFields.collect {
            case field if isRequiredInput(field) && !names.contains(field.name) =>
              s"Required input field '${field.name}' is not selected."
          }
      def values: List[String]  = fields.flatMap { field =>
        inputFieldDefinition(inputObject, field.name).fold(
          List(s"Field '${field.name}' is not defined on input type '${render(input)}'.")
        )(definition => selected(field.value, definition._type, scope))
      }

      if (inputObject.kind != __TypeKind.INPUT_OBJECT)
        repeated :+ s"Input type '${render(input)}' is not an input object type."
      else repeated ::: missing ::: values
    }

    private def list(element: SelectedValue, input: __Type, output: __Type): List[String] =
      (listItem(input), listItem(output)) match {
        case (Some(inputItem), Some(outputItem)) => selected(element, inputItem, outputItem)
        case _                                   =>
          List(
            s"List selection needs list types, found input type '${render(input)}' and selected type '${render(output)}'."
          )
      }

    private def listItem(tpe: __Type): Option[__Type] =
      Some(nullableType(tpe)).filter(_.kind == __TypeKind.LIST).flatMap(_.ofType)

    private def render(tpe: __Type): String = DocumentRenderer.renderTypeName(tpe)
  }
}
