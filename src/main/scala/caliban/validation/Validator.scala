package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Document, OperationType, Selection }
import caliban.schema.{ RootType, Types }
import caliban.schema.Types.{ __Type, __TypeKind, DeprecatedArgs }
import caliban.Rendering
import zio.IO

object Validator {

  def validate(document: Document, rootType: RootType): IO[ValidationError, Unit] = {
    val operations = collectOperations(document)
    for {
      _                 <- validateOperationNameUniqueness(operations)
      _                 <- validateLoneAnonymousOperation(operations)
      _                 <- validateSubscriptionOperation(operations)
      typesForFragments = collectTypesValidForFragments(rootType)
      _                 <- validateDocumentFields(document, rootType, typesForFragments)
    } yield ()
  }

  private def collectOperations(document: Document): List[OperationDefinition] =
    document.definitions.collect { case o: OperationDefinition => o }

  private def collectTypesValidForFragments(rootType: RootType): Map[String, __Type] =
    rootType.types.filter {
      case (_, t) => t.kind == __TypeKind.OBJECT || t.kind == __TypeKind.INTERFACE || t.kind == __TypeKind.UNION
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
                IO.fail(ValidationError("Mutation operations are not supported on this schema", ""))
              )(validateFields(selectionSet, _))
            case OperationType.Subscription =>
              rootType.subscriptionType.fold[IO[ValidationError, Unit]](
                IO.fail(ValidationError("Subscription operations are not supported on this schema", ""))
              )(validateFields(selectionSet, _))
          }
        case FragmentDefinition(name, typeCondition, _, selectionSet) =>
          typesForFragments
            .get(typeCondition.name)
            .map(validateFields(selectionSet, _))
            .getOrElse(
              IO.fail(
                ValidationError(
                  s"Fragment '$name' targets an invalid type: '${typeCondition.name}'",
                  "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
                )
              )
            )
      }
      .unit

  private def validateFields(selectionSet: List[Selection], currentType: __Type): IO[ValidationError, Unit] =
    IO.foreach(selectionSet) {
        case f: Field          => validateField(f, currentType)
        case _: FragmentSpread => IO.unit
        case InlineFragment(typeCondition, _, selectionSet) =>
          validateFields(
            selectionSet,
            typeCondition
              .flatMap(onType => currentType.possibleTypes.getOrElse(Nil).find(_.name.contains(onType.name)))
              .getOrElse(currentType)
          )
      }
      .unit

  private def validateField(field: Field, currentType: __Type): IO[ValidationError, Unit] =
    IO.fromOption(currentType.fields(DeprecatedArgs(Some(true))).getOrElse(Nil).find(_.name == field.name))
      .mapError(
        _ =>
          ValidationError(
            s"Field '${field.name}' does not exist on type '${Rendering.renderTypeName(currentType)}'",
            "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
          )
      )
      .flatMap(f => validateFields(field.selectionSet, Types.innerType(f.`type`())))

  private def validateOperationNameUniqueness(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val names         = operations.flatMap(_.name).groupBy(identity)
    val repeatedNames = names.collect { case (name, items) if items.length > 1 => name }
    IO.when(repeatedNames.nonEmpty)(
      IO.fail(
        ValidationError(
          s"Multiple operations have the same name: ${repeatedNames.mkString(", ")}",
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
          "Found both anonymous and named operations",
          "GraphQL allows a short‐hand form for defining query operations when only that one operation exists in the document."
        )
      )
    )
  }

  private def validateSubscriptionOperation(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val subscriptions = operations.filter(_.operationType == OperationType.Subscription)
    // TODO handle fragments
    IO.fromOption(subscriptions.find(_.selectionSet.length > 1))
      .map(
        op =>
          ValidationError(
            s"Subscription ${op.name.map(n => s"'$n'").getOrElse("")} has more than one root field",
            "Subscription operations must have exactly one root field."
          )
      )
      .flip
  }

}
