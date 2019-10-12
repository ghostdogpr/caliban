package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.Rendering
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Value.NullValue
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.schema.{ RootType, Types }
import zio.IO

object Validator {

  /**
   * Verifies that the given document is valid for this type. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validate(document: Document, rootType: RootType): IO[ValidationError, Unit] = {
    val (operations, fragments) = collectOperationsAndFragments(document)
    for {
      _                 <- validateOperationNameUniqueness(operations)
      _                 <- validateLoneAnonymousOperation(operations)
      fragmentMap       <- validateFragments(fragments)
      selectionSets     = collectSelectionSets(operations.flatMap(_.selectionSet) ++ fragments.flatMap(_.selectionSet))
      _                 <- validateDirectives(selectionSets)
      _                 <- validateVariables(operations)
      _                 <- validateFragmentSpreads(selectionSets, fragments)
      _                 <- validateSubscriptionOperation(operations, fragmentMap)
      typesForFragments = collectTypesValidForFragments(rootType)
      _                 <- validateDocumentFields(document, rootType, typesForFragments)
    } yield ()
  }

  private def collectOperationsAndFragments(
    document: Document
  ): (List[OperationDefinition], List[FragmentDefinition]) =
    document.definitions.foldLeft((List.empty[OperationDefinition], List.empty[FragmentDefinition])) {
      case ((operations, fragments), o: OperationDefinition) => (o :: operations, fragments)
      case ((operations, fragments), f: FragmentDefinition)  => (operations, f :: fragments)
    }

  private def collectTypesValidForFragments(rootType: RootType): Map[String, __Type] =
    rootType.types.filter {
      case (_, t) => t.kind == __TypeKind.OBJECT || t.kind == __TypeKind.INTERFACE || t.kind == __TypeKind.UNION
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

  private def validateVariables(definitions: List[OperationDefinition]): IO[ValidationError, Unit] =
    IO.foreach(definitions)(
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
          }
      )
      .unit

  private def collectFragmentSpreads(selectionSet: List[Selection]): List[FragmentSpread] =
    selectionSet.collect { case f: FragmentSpread => f }

  private def validateFragmentSpreads(
    selectionSets: List[Selection],
    fragments: List[FragmentDefinition]
  ): IO[ValidationError, Unit] = {
    val spreads       = collectFragmentSpreads(selectionSets)
    val spreadNames   = spreads.map(_.name).toSet
    val fragmentNames = fragments.map(_.name).toSet
    IO.foreach(fragments)(
      f =>
        if (!spreadNames.contains(f.name))
          IO.fail(
            ValidationError(
              s"Fragment '${f.name}' is not used in any spread.",
              "Defined fragments must be used within a document."
            )
          )
        else if (detectCycles(f, fragments))
          IO.fail(
            ValidationError(
              s"Fragment '${f.name}' forms a cycle.",
              "The graph of fragment spreads must not form any cycles including spreading itself. Otherwise an operation could infinitely spread or infinitely execute on cycles in the underlying data."
            )
          )
        else IO.unit
    ) *> IO
      .foreach(spreadNames)(
        spread =>
          IO.when(!fragmentNames.contains(spread))(
            IO.fail(
              ValidationError(
                s"Fragment spread '$spread' is not defined.",
                "Named fragment spreads must refer to fragments defined within the document. It is a validation error if the target of a spread is not defined."
              )
            )
          )
      )
      .unit
  }

  private def detectCycles(
    fragment: FragmentDefinition,
    fragments: List[FragmentDefinition],
    visited: Set[String] = Set()
  ): Boolean = {
    val selectionSets     = collectSelectionSets(fragment.selectionSet)
    val descendantSpreads = collectFragmentSpreads(selectionSets)
    descendantSpreads.exists(
      s =>
        visited.contains(s.name) ||
          fragments.find(_.name == s.name).fold(false)(f => detectCycles(f, fragments, visited + s.name))
    )
  }

  private def validateDocumentFields(
    document: Document,
    rootType: RootType,
    typesForFragments: Map[String, __Type]
  ): IO[ValidationError, Unit] =
    IO.foreach(document.definitions) {
        case OperationDefinition(opType, _, _, _, selectionSet) =>
          opType match {
            case OperationType.Query => validateFields(selectionSet, rootType.queryType)
            case OperationType.Mutation =>
              rootType.mutationType.fold[IO[ValidationError, Unit]](
                IO.fail(ValidationError("Mutation operations are not supported on this schema.", ""))
              )(validateFields(selectionSet, _))
            case OperationType.Subscription =>
              rootType.subscriptionType.fold[IO[ValidationError, Unit]](
                IO.fail(ValidationError("Subscription operations are not supported on this schema.", ""))
              )(validateFields(selectionSet, _))
          }
        case FragmentDefinition(name, typeCondition, _, selectionSet) =>
          IO.fromOption(typesForFragments.get(typeCondition.name))
            .mapError(
              _ =>
                ValidationError(
                  s"Fragment '$name' targets an invalid type: '${typeCondition.name}'.",
                  "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
                )
            )
            .flatMap(t => validateFields(selectionSet, t) *> validateFragmentType(Some(name), t))
      }
      .unit

  private def validateFields(selectionSet: List[Selection], currentType: __Type): IO[ValidationError, Unit] =
    IO.foreach(selectionSet) {
      case f: Field          => validateField(f, currentType)
      case _: FragmentSpread => IO.unit
      case InlineFragment(typeCondition, _, selectionSet) =>
        (typeCondition match {
          case Some(onType) =>
            IO.fromOption(currentType.possibleTypes.getOrElse(Nil).find(_.name.contains(onType.name)))
              .mapError(_ => ValidationError(s"Inline Fragment on invalid type '${onType.name}'.", ""))
          case None => IO.succeed(currentType)
        }).flatMap(t => validateFields(selectionSet, t) *> validateFragmentType(None, t))
    } *> validateLeafFieldSelection(selectionSet, currentType)

  private def validateField(field: Field, currentType: __Type): IO[ValidationError, Unit] =
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
          validateFields(field.selectionSet, Types.innerType(f.`type`())) *>
            validateArguments(field, f, currentType)
        }
    }

  private def validateArguments(field: Field, f: __Field, currentType: __Type): IO[ValidationError, List[Unit]] =
    IO.foreach(field.arguments.keys)(
      arg =>
        IO.when(!f.args.exists(_.name == arg))(
          IO.fail(
            ValidationError(
              s"Argument '$arg' is not defined on field '${field.name}' of type '${currentType.name.getOrElse("")}'.",
              "Every argument provided to a field or directive must be defined in the set of possible arguments of that field or directive."
            )
          )
        )
    ) *>
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

  private def validateSubscriptionOperation(
    operations: List[OperationDefinition],
    fragments: Map[String, FragmentDefinition]
  ): IO[ValidationError, Unit] =
    IO.fromOption(
        operations
          .filter(_.operationType == OperationType.Subscription)
          .find(op => Executor.mergeSelectionSet(op.selectionSet, "", fragments, Map()).length > 1)
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
            s"Fragment '${name.getOrElse("inline")}' is defined on invalid type '${targetType.name.getOrElse("")}'",
            "Fragments can only be declared on unions, interfaces, and objects. They are invalid on scalars. They can only be applied on non‐leaf fields. This rule applies to both inline and named fragments."
          )
        )
    }

}
