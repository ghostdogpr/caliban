package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.Rendering
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Document, OperationType, Selection }
import caliban.schema.Types.{ Type, TypeKind }
import caliban.schema.{ Schema, Types }
import zio.IO

object Validator {

  def validate[G](document: Document, schema: Schema[G]): IO[ValidationError, Unit] = {
    val operations = collectOperations(document)
    val schemaType = schema.toType
    for {
      _                 <- validateOperationNameUniqueness(operations)
      _                 <- validateLoneAnonymousOperation(operations)
      _                 <- validateSubscriptionOperation(operations)
      typesForFragments = collectTypesValidForFragments(schemaType)
      _                 <- validateDocumentFields(document, schemaType, typesForFragments)
    } yield ()
  }

  private def collectOperations(document: Document): List[OperationDefinition] =
    document.definitions.collect { case o: OperationDefinition => o }

  private def collectTypesValidForFragments(currentType: Type): Map[String, Type] =
    Types
      .collectTypes(currentType)
      .collect {
        case t @ Type(TypeKind.OBJECT, Some(name), _, _, _, _, _)    => name -> t
        case t @ Type(TypeKind.INTERFACE, Some(name), _, _, _, _, _) => name -> t
        case t @ Type(TypeKind.UNION, Some(name), _, _, _, _, _)     => name -> t
      }
      .toMap

  private def validateDocumentFields(
    document: Document,
    schemaType: Type,
    typesForFragments: Map[String, Type]
  ): IO[ValidationError, Unit] =
    IO.foreach(document.definitions) {
        case OperationDefinition(_, _, _, _, selectionSet) =>
          validateFields(selectionSet, schemaType)
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

  private def validateFields(selectionSet: List[Selection], currentType: Type): IO[ValidationError, Unit] =
    IO.foreach(selectionSet) {
        case f: Field          => validateField(f, currentType)
        case _: FragmentSpread => IO.unit
        case InlineFragment(typeCondition, _, selectionSet) =>
          validateFields(
            selectionSet,
            typeCondition
              .flatMap(onType => currentType.subTypes.find(_.name.contains(onType.name)))
              .getOrElse(currentType)
          )
      }
      .unit

  private def validateField(field: Field, currentType: Type): IO[ValidationError, Unit] =
    IO.fromOption(currentType.fields.find(_.name == field.name))
      .mapError(
        _ =>
          ValidationError(
            s"Field '${field.name}' does not exist on type '${Rendering.renderTypeName(currentType)}'",
            "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
          )
      )
      .flatMap(f => validateFields(field.selectionSet, Types.innerType(f.`type`)))

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
