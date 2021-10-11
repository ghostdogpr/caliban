package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.{ RootSchema, RootSchemaBuilder, RootType, Types }
import caliban.{ InputValue, Rendering, Value }
import zio.IO
import Utils._

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] = {
    val fieldsAndfragments = FieldsAndFragments(
      context,
      parentType,
      selectionSet
    )

    val conflicts = collectConflictsWithin(
      context,
      fieldsAndfragments.fields
    ) ++
      fieldsAndfragments.fragmentNames.flatMap { name =>
        collectConflictsBetweenFieldsAndFragment(
          context,
          false,
          fieldsAndfragments.fields,
          name
        ) ++ fieldsAndfragments.fragmentNames.flatMap { name2 =>
          // TODO: can skip stuff here by using index + drop.
          collectConflictsBetweenFragments(context, false, name, name2)
        }
      }

    conflicts match {
      case head :: _ => IO.fail(ValidationError(head, ""))
      case Nil       => IO.unit
    }
  }

  def collectConflictsWithin(
    context: Context,
    fields: FieldMap
  ): Iterable[String] =
    fields
      .collect({
        case (name, field) if field.size > 0 =>
          findConflict(context, false, field, field)
      })
      .flatten

  def cross[A](a: Iterable[A]): Iterable[(A, A)]                 =
    for (xs <- a; ys <- a) yield (xs, ys)

  def cross[A](a: Iterable[A], b: Iterable[A]): Iterable[(A, A)] =
    for (xs <- a; ys <- b) yield (xs, ys)

  def doTypesConflict(t1: __Type, t2: __Type): Boolean           =
    t1.toType(!t1.isNullable) != t2.toType(!t2.isNullable)

  // TODO: get field name for better error message
  def findConflict(
    context: Context,
    parentFieldsAreMutuallyExclusive: Boolean,
    f1: Set[SelectedField],
    f2: Set[SelectedField]
  ): Iterable[String] =
    cross(f1, f2).flatMap { pair =>
      val (f1, f2)             = pair
      val areMutuallyExclusive = parentFieldsAreMutuallyExclusive || (f1.parentType != f2.parentType &&
        isObjectType(f1.parentType) &&
        isObjectType(f2.parentType))

      if (!areMutuallyExclusive && f1.fieldDef.name != f2.fieldDef.name)
        List(
          s"${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name.getOrElse("")}.${f2.fieldDef.name} are different fields."
        )
      else if (f1.selection.arguments != f2.selection.arguments)
        List(s"${f1.fieldDef.name} and ${f2.fieldDef.name} have different arguments")
      else if (doTypesConflict(f1.fieldDef.`type`(), f2.fieldDef.`type`()))
        List(
          s"${f1.fieldDef.name} and ${f2.fieldDef.name} have different types, ${f1.fieldDef.`type`()}, ${f2.fieldDef.`type`()}."
        )
      else
        findConflictsBetweenSubSelectionSets(
          context,
          areMutuallyExclusive,
          f1.fieldDef,
          f1.selection,
          f2.fieldDef,
          f2.selection
        )
    }

  def findConflictsBetweenSubSelectionSets(
    context: Context,
    areMutuallyExclusive: Boolean,
    def1: __Field,
    sel1: Field,
    def2: __Field,
    sel2: Field
  ): Iterable[String] = {
    val fieldsAndFragments1 = FieldsAndFragments(context, def1.`type`(), sel1.selectionSet)
    val fieldsAndFragments2 = FieldsAndFragments(context, def2.`type`(), sel2.selectionSet)

    collectConflictsBetween(context, areMutuallyExclusive, fieldsAndFragments1.fields, fieldsAndFragments2.fields) ++
      collectConflictsBetween(context, areMutuallyExclusive, fieldsAndFragments2.fields, fieldsAndFragments1.fields) ++
      cross(fieldsAndFragments1.fragmentNames, fieldsAndFragments2.fragmentNames).flatMap {
        case (fragment1, fragment2) =>
          collectConflictsBetweenFragments(
            context,
            areMutuallyExclusive,
            fragment1,
            fragment2
          )
      }
  }

  def collectConflictsBetweenFragments(
    context: Context,
    areMutuallyExclusive: Boolean,
    fragmentName1: String,
    fragmentName2: String
  ): Iterable[String] =
    // check self
    // memoize bla bla
    // zip mapN
    context.fragments
      .get(fragmentName1)
      .flatMap(f1 => context.fragments.get(fragmentName2).map(f2 => (f1, f2)))
      .flatMap { case (fragment1, fragment2) =>
        getReferencedFieldsAndFragmentNames(context, fragment1)
          .flatMap(f1 => getReferencedFieldsAndFragmentNames(context, fragment2).map(f2 => (f1, f2)))
          .map { case (fieldsAndFragments1, fieldsAndFragments2) =>
            val c1 =
              collectConflictsBetween(
                context,
                areMutuallyExclusive,
                fieldsAndFragments1.fields,
                fieldsAndFragments2.fields
              )

            val c2 = fieldsAndFragments2.fragmentNames.toList
              .flatMap(fragmentName =>
                collectConflictsBetweenFieldsAndFragment(
                  context,
                  areMutuallyExclusive,
                  fieldsAndFragments1.fields,
                  fragmentName
                )
              )

            val c3 = fieldsAndFragments1.fragmentNames.toList.flatMap(fragmentName =>
              collectConflictsBetweenFieldsAndFragment(
                context,
                areMutuallyExclusive,
                fieldsAndFragments2.fields,
                fragmentName
              )
            )

            c1 ++ c2 ++ c3
          }
      }
      .getOrElse(List.empty)

  def collectConflictsBetween(
    context: Context,
    areMutuallyExclusive: Boolean,
    fieldMap1: FieldMap,
    fieldMap2: FieldMap
  ): Iterable[String] =
    fieldMap1.toIterable
      .map({ case (name, fields1) =>
        fieldMap2
          .get(name)
          .map(fields2 => findConflict(context, areMutuallyExclusive, fields1, fields2))
          .getOrElse(List.empty)
      })
      .flatten

  def collectConflictsBetweenFieldsAndFragment(
    context: Context,
    areMutuallyExclusive: Boolean,
    fields1: FieldMap,
    fragmentName: String
  ): Iterable[String] =
    context.fragments
      .get(fragmentName)
      .flatMap { fragment =>
        getType(fragment.typeCondition, context).map { typ =>
          FieldsAndFragments(context, typ, fragment.selectionSet)
        }
      }
      .map { fieldsAndFragments2 =>
        if (fields1 != fieldsAndFragments2.fields) {
          collectConflictsBetween(context, areMutuallyExclusive, fields1, fieldsAndFragments2.fields)
        } else
          fieldsAndFragments2.fragmentNames.flatMap { fragmentName =>
            collectConflictsBetweenFieldsAndFragment(context, areMutuallyExclusive, fields1, fragmentName)
          }
      }
      .getOrElse(List.empty)

  def getReferencedFieldsAndFragmentNames(
    context: Context,
    fragment: FragmentDefinition
  ) =
    getType(fragment.typeCondition, context).map { t =>
      FieldsAndFragments(context, t, fragment.selectionSet)
    }

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))
}
