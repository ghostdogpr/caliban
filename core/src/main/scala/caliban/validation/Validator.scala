package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.Rendering
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.Value.{ NullValue, VariableValue }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection, Type, Value }
import caliban.schema.{ RootType, Types }
import zio.IO

object Validator {

  case class Context(
    document: Document,
    rootType: RootType,
    operations: List[OperationDefinition],
    fragments: Map[String, FragmentDefinition],
    selectionSets: List[Selection]
  )

  /**
   * Verifies that the given document is valid for this type. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validate(document: Document, rootType: RootType): IO[ValidationError, Unit] = {
    val (operations, fragments) = collectOperationsAndFragments(document)
    for {
      fragmentMap   <- validateFragments(fragments)
      selectionSets = collectSelectionSets(operations.flatMap(_.selectionSet) ++ fragments.flatMap(_.selectionSet))
      context       = Context(document, rootType, operations, fragmentMap, selectionSets)
      _             <- validateOperationNameUniqueness(operations)
      _             <- validateLoneAnonymousOperation(operations)
      _             <- validateDirectives(selectionSets)
      _             <- validateFragmentSpreads(context)
      _             <- validateVariables(context)
      _             <- validateSubscriptionOperation(context)
      _             <- validateDocumentFields(context)
    } yield ()
  }

  private def collectOperationsAndFragments(document: Document): (List[OperationDefinition], List[FragmentDefinition]) =
    document.definitions.foldLeft((List.empty[OperationDefinition], List.empty[FragmentDefinition])) {
      case ((operations, fragments), o: OperationDefinition) => (o :: operations, fragments)
      case ((operations, fragments), f: FragmentDefinition)  => (operations, f :: fragments)
    }

  private def collectVariablesUsed(context: Context, selectionSet: List[Selection]): Set[String] = {
    def collectValues(selectionSet: List[Selection]): List[Value] =
      selectionSet.flatMap {
        case FragmentSpread(name, directives) =>
          directives.flatMap(_.arguments.values) ++ context.fragments
            .get(name)
            .map(f => f.directives.flatMap(_.arguments.values) ++ collectValues(f.selectionSet))
            .getOrElse(Nil)
        case Field(_, _, arguments, directives, selectionSet) =>
          arguments.values ++ directives.flatMap(_.arguments.values) ++ collectValues(selectionSet)
        case InlineFragment(_, directives, selectionSet) =>
          directives.flatMap(_.arguments.values) ++ collectValues(selectionSet)
      }
    collectValues(selectionSet).collect {
      case VariableValue(name) => name
    }.toSet
  }

  private def collectSelectionSets(selectionSet: List[Selection]): List[Selection] =
    selectionSet ++ selectionSet.flatMap {
      case _: FragmentSpread                  => Nil
      case Field(_, _, _, _, selectionSet)    => collectSelectionSets(selectionSet)
      case InlineFragment(_, _, selectionSet) => collectSelectionSets(selectionSet)
    }

  private def collectDirectives(selectionSet: List[Selection]): List[Directive] =
    selectionSet.flatMap {
      case FragmentSpread(_, directives)    => directives
      case Field(_, _, _, directives, _)    => directives
      case InlineFragment(_, directives, _) => directives
    }

  private def validateDirectives(selectionSet: List[Selection]): IO[ValidationError, Unit] = {
    val directives          = collectDirectives(selectionSet)
    val supportedDirectives = Introspector.directives.map(_.name).toSet
    IO.foreach(directives)(
        d =>
          IO.when(!supportedDirectives.contains(d.name))(
            IO.fail(
              ValidationError(
                s"Directive '${d.name}' is not supported.",
                "GraphQL servers define what directives they support. For each usage of a directive, the directive must be available on that server."
              )
            )
          )
      )
      .unit

  }

  private def validateVariables(context: Context): IO[ValidationError, Unit] =
    IO.foreach(context.operations)(
        op =>
          IO.foreach(op.variableDefinitions.groupBy(_.name)) {
            case (name, variables) =>
              IO.when(variables.length > 1)(
                IO.fail(
                  ValidationError(
                    s"Variable '$name' is defined more than once.",
                    "If any operation defines more than one variable with the same name, it is ambiguous and invalid. It is invalid even if the type of the duplicate variable is the same."
                  )
                )
              )
          } *> IO.foreach(op.variableDefinitions) { v =>
            val t = Type.innerType(v.variableType)
            IO.whenCase(context.rootType.types.get(t).map(_.kind)) {
              case Some(__TypeKind.OBJECT) | Some(__TypeKind.UNION) | Some(__TypeKind.INTERFACE) =>
                IO.fail(
                  ValidationError(
                    s"Type of variable '${v.name}' is not a valid input type.",
                    "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
                  )
                )
            }
          } *> {
            val variableUsages = collectVariablesUsed(context, op.selectionSet)
            IO.foreach(variableUsages)(
              v =>
                IO.when(!op.variableDefinitions.exists(_.name == v))(
                  IO.fail(
                    ValidationError(
                      s"Variable '$v' is not defined.",
                      "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
                    )
                  )
                )
            ) *> IO.foreach(op.variableDefinitions)(
              v =>
                IO.when(!variableUsages.contains(v.name))(
                  IO.fail(
                    ValidationError(
                      s"Variable '${v.name}' is not used.",
                      "All variables defined by an operation must be used in that operation or a fragment transitively included by that operation. Unused variables cause a validation error."
                    )
                  )
                )
            )
          }
      )
      .unit

  private def collectFragmentSpreads(selectionSet: List[Selection]): List[FragmentSpread] =
    selectionSet.collect { case f: FragmentSpread => f }

  private def validateFragmentSpreads(context: Context): IO[ValidationError, Unit] = {
    val spreads     = collectFragmentSpreads(context.selectionSets)
    val spreadNames = spreads.map(_.name).toSet
    IO.foreach(context.fragments.values)(
        f =>
          if (!spreadNames.contains(f.name))
            IO.fail(
              ValidationError(
                s"Fragment '${f.name}' is not used in any spread.",
                "Defined fragments must be used within a document."
              )
            )
          else if (detectCycles(context, f))
            IO.fail(
              ValidationError(
                s"Fragment '${f.name}' forms a cycle.",
                "The graph of fragment spreads must not form any cycles including spreading itself. Otherwise an operation could infinitely spread or infinitely execute on cycles in the underlying data."
              )
            )
          else IO.unit
      )
      .unit
  }

  private def detectCycles(context: Context, fragment: FragmentDefinition, visited: Set[String] = Set()): Boolean = {
    val selectionSets     = collectSelectionSets(fragment.selectionSet)
    val descendantSpreads = collectFragmentSpreads(selectionSets)
    descendantSpreads.exists(
      s =>
        visited.contains(s.name) ||
          context.fragments.get(s.name).fold(false)(f => detectCycles(context, f, visited + s.name))
    )
  }

  private def validateDocumentFields(context: Context): IO[ValidationError, Unit] =
    IO.foreach(context.document.definitions) {
        case OperationDefinition(opType, _, _, _, selectionSet) =>
          opType match {
            case OperationType.Query => validateFields(context, selectionSet, context.rootType.queryType)
            case OperationType.Mutation =>
              context.rootType.mutationType.fold[IO[ValidationError, Unit]](
                IO.fail(ValidationError("Mutation operations are not supported on this schema.", ""))
              )(validateFields(context, selectionSet, _))
            case OperationType.Subscription =>
              context.rootType.subscriptionType.fold[IO[ValidationError, Unit]](
                IO.fail(ValidationError("Subscription operations are not supported on this schema.", ""))
              )(validateFields(context, selectionSet, _))
          }
        case _: FragmentDefinition => IO.unit
      }
      .unit

  private def validateFields(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  ): IO[ValidationError, Unit] =
    IO.foreach(selectionSet) {
      case f: Field => validateField(context, f, currentType)
      case FragmentSpread(name, _) =>
        context.fragments.get(name) match {
          case None =>
            IO.fail(
              ValidationError(
                s"Fragment spread '$name' is not defined.",
                "Named fragment spreads must refer to fragments defined within the document. It is a validation error if the target of a spread is not defined."
              )
            )
          case Some(fragment) =>
            validateSpread(context, Some(name), currentType, Some(fragment.typeCondition), fragment.selectionSet)
        }
      case InlineFragment(typeCondition, _, selectionSet) =>
        validateSpread(context, None, currentType, typeCondition, selectionSet)
    } *> validateLeafFieldSelection(selectionSet, currentType)

  private def validateSpread(
    context: Context,
    name: Option[String],
    currentType: __Type,
    typeCondition: Option[NamedType],
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] =
    typeCondition.fold[Option[__Type]](Some(currentType))(t => context.rootType.types.get(t.name)) match {
      case Some(fragmentType) =>
        validateFragmentType(name, fragmentType) *> {
          val possibleTypes         = getPossibleTypes(currentType).flatMap(_.name)
          val possibleFragmentTypes = getPossibleTypes(fragmentType).flatMap(_.name)
          val applicableTypes       = possibleTypes intersect possibleFragmentTypes
          IO.when(applicableTypes.isEmpty)(
            IO.fail(
              ValidationError(
                s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} is not possible: possible types are '${possibleTypes
                  .mkString(", ")}' and possible fragment types are '${possibleFragmentTypes.mkString(", ")}'.",
                "Fragments are declared on a type and will only apply when the runtime object type matches the type condition. They also are spread within the context of a parent type. A fragment spread is only valid if its type condition could ever apply within the parent type."
              )
            )
          ) *> validateFields(context, selectionSet, fragmentType)
        }
      case None =>
        IO.fail(
          ValidationError(
            s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} targets an invalid type: '${typeCondition.map(_.name).getOrElse("?")}'.",
            "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
          )
        )
    }

  private def getPossibleTypes(t: __Type): List[__Type] =
    t.kind match {
      case __TypeKind.OBJECT                       => List(t)
      case __TypeKind.INTERFACE | __TypeKind.UNION => t.possibleTypes.getOrElse(Nil)
      case _                                       => Nil
    }

  private def validateField(context: Context, field: Field, currentType: __Type): IO[ValidationError, Unit] =
    IO.when(field.name != "__typename") {
      IO.fromOption(currentType.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).find(_.name == field.name))
        .mapError(
          _ =>
            ValidationError(
              s"Field '${field.name}' does not exist on type '${Rendering.renderTypeName(currentType)}'.",
              "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
            )
        )
        .flatMap { f =>
          validateFields(context, field.selectionSet, Types.innerType(f.`type`())) *>
            validateArguments(field, f, currentType)
        }
    }

  private def validateArguments(field: Field, f: __Field, currentType: __Type): IO[ValidationError, List[Unit]] =
    IO.foreach(field.arguments) {
      case (arg, argValue) =>
        f.args.find(_.name == arg) match {
          case None =>
            IO.fail(
              ValidationError(
                s"Argument '$arg' is not defined on field '${field.name}' of type '${currentType.name.getOrElse("")}'.",
                "Every argument provided to a field or directive must be defined in the set of possible arguments of that field or directive."
              )
            )
          case Some(inputValue) => validateInputValues(inputValue, argValue)
        }
    } *>
      IO.foreach(f.args.filter(a => a.`type`().kind == __TypeKind.NON_NULL && a.defaultValue.isEmpty))(
        arg =>
          IO.when(field.arguments.get(arg.name).forall(_ == NullValue))(
            IO.fail(
              ValidationError(
                s"Required argument '${arg.name}' is null or missing on field '${field.name}' of type '${currentType.name
                  .getOrElse("")}'.",
                "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
              )
            )
          )
      )

  private def validateInputValues(inputValue: __InputValue, argValue: Value): IO[ValidationError, Unit] = {
    val t           = inputValue.`type`()
    val inputType   = if (t.kind == __TypeKind.NON_NULL) t.ofType.getOrElse(t) else t
    val inputFields = inputType.inputFields.getOrElse(Nil)
    argValue match {
      case Value.ObjectValue(fields) =>
        IO.foreach(fields) {
          case (k, v) =>
            inputFields.find(_.name == k) match {
              case None =>
                IO.fail(
                  ValidationError(
                    s"Input field '$k' is not defined on type '${inputType.name.getOrElse("?")}'.",
                    "Every input field provided in an input object value must be defined in the set of possible fields of that input object’s expected type."
                  )
                )
              case Some(value) => validateInputValues(value, v)
            }
        } *> IO
          .foreach(inputFields)(
            inputField =>
              IO.when(
                inputField.defaultValue.isEmpty &&
                  inputField.`type`().kind == __TypeKind.NON_NULL &&
                  !fields.contains(inputField.name)
              )(
                IO.fail(
                  ValidationError(
                    s"Required field '${inputField.name}' on object '${inputType.name.getOrElse("?")}' was not provided.",
                    "Input object fields may be required. Much like a field may have required arguments, an input object may have required fields. An input field is required if it has a non‐null type and does not have a default value. Otherwise, the input object field is optional."
                  )
                )
              )
          )
          .unit
      case _ => IO.unit
    }
  }

  private def validateLeafFieldSelection(selections: List[Selection], currentType: __Type): IO[ValidationError, Unit] =
    IO.whenCase(currentType.kind) {
      case __TypeKind.SCALAR | __TypeKind.ENUM if selections.nonEmpty =>
        IO.fail(
          ValidationError(
            s"Field selection is impossible on type '${currentType.name.getOrElse("")}'.",
            "Field selections on scalars or enums are never allowed, because they are the leaf nodes of any GraphQL query."
          )
        )
      case __TypeKind.INTERFACE | __TypeKind.UNION | __TypeKind.OBJECT if selections.isEmpty =>
        IO.fail(
          ValidationError(
            s"Field selection is mandatory on type '${currentType.name.getOrElse("")}'.",
            "Leaf selections on objects, interfaces, and unions without subfields are disallowed."
          )
        )
    }

  private def validateOperationNameUniqueness(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val names         = operations.flatMap(_.name).groupBy(identity)
    val repeatedNames = names.collect { case (name, items) if items.length > 1 => name }
    IO.when(repeatedNames.nonEmpty)(
      IO.fail(
        ValidationError(
          s"Multiple operations have the same name: ${repeatedNames.mkString(", ")}.",
          "Each named operation definition must be unique within a document when referred to by its name."
        )
      )
    )
  }

  private def validateLoneAnonymousOperation(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val anonymous = operations.filter(_.name.isEmpty)
    IO.when(operations.length > 1 && anonymous.nonEmpty)(
      IO.fail(
        ValidationError(
          "Found both anonymous and named operations.",
          "GraphQL allows a short‐hand form for defining query operations when only that one operation exists in the document."
        )
      )
    )
  }

  private def validateFragments(
    fragments: List[FragmentDefinition]
  ): IO[ValidationError, Map[String, FragmentDefinition]] =
    IO.foldLeft(fragments)(Map.empty[String, FragmentDefinition]) {
      case (fragmentMap, fragment) =>
        if (fragmentMap.contains(fragment.name)) {
          IO.fail(
            ValidationError(
              s"Fragment '${fragment.name}' is defined more than once.",
              "Fragment definitions are referenced in fragment spreads by name. To avoid ambiguity, each fragment’s name must be unique within a document."
            )
          )
        } else IO.succeed(fragmentMap.updated(fragment.name, fragment))
    }

  private def validateSubscriptionOperation(context: Context): IO[ValidationError, Unit] =
    IO.fromOption(
        context.operations
          .filter(_.operationType == OperationType.Subscription)
          .find(op => Executor.mergeSelectionSet(op.selectionSet, "", context.fragments, Map()).length > 1)
      )
      .map(
        op =>
          ValidationError(
            s"Subscription ${op.name.map(n => s"'$n'").getOrElse("")} has more than one root field.",
            "Subscription operations must have exactly one root field."
          )
      )
      .flip

  private def validateFragmentType(name: Option[String], targetType: __Type): IO[ValidationError, Unit] =
    targetType.kind match {
      case __TypeKind.UNION | __TypeKind.INTERFACE | __TypeKind.OBJECT => IO.unit
      case _ =>
        IO.fail(
          ValidationError(
            s"${name.fold("Inline fragment")(n => s"Fragment '$n'")} is defined on invalid type '${targetType.name
              .getOrElse("")}'",
            "Fragments can only be declared on unions, interfaces, and objects. They are invalid on scalars. They can only be applied on non‐leaf fields. This rule applies to both inline and named fragments."
          )
        )
    }

}
