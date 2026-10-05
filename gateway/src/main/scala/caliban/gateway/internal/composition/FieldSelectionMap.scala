package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.InputValue.{ ListValue, ObjectValue, VariableValue }
import caliban.gateway._
import caliban.introspection.adt._
import caliban.parsing.parsers.Parsers.{ name, value, whitespace }
import caliban.rendering.DocumentRenderer
import fastparse._

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

  def validate(
    value: SelectedValue,
    inputType: __Type,
    outputType: __Type,
    types: Map[String, __Type]
  ): List[String] =
    new Validation(types).selected(value, inputType, outputType)

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
        tpe    <- validated(errors, Right(field._type))
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
