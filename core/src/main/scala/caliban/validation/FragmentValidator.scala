package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import zio.{ IO, UIO }
import Utils._
import Utils.syntax._

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] = {
    val shapeCache   = scala.collection.mutable.Map.empty[Iterable[Selection], Iterable[String]]
    val parentsCache = scala.collection.mutable.Map.empty[Iterable[Selection], Iterable[String]]
    val groupsCache  = scala.collection.mutable.Map.empty[Set[SelectedField], Iterable[Set[SelectedField]]]

    def sameResponseShapeByName(context: Context, parentType: __Type, set: Iterable[Selection]): Iterable[String] =
      if (shapeCache.get(set).isDefined) { shapeCache.get(set).get }
      else {
        val fields = FieldMap(context, parentType, set)
        val res    = fields.flatMap { case (name, values) =>
          cross(values).flatMap { pair =>
            val (f1, f2) = pair
            if (doTypesConflict(f1.fieldDef.`type`(), f2.fieldDef.`type`())) {
              List(
                s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                  .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
              )
            } else
              sameResponseShapeByName(context, parentType, f1.selection.selectionSet ++ f2.selection.selectionSet)
          }
        }
        shapeCache.put(set, res)
        res
      }

    def sameForCommonParentsByName(context: Context, parentType: __Type, set: Iterable[Selection]): Iterable[String] =
      if (parentsCache.get(set).isDefined) { parentsCache.get(set).get }
      else {
        val fields = FieldMap(context, parentType, set)
        val res    = fields.flatMap({ case (name, fields) =>
          groupByCommonParents(context, parentType, fields).flatMap { group =>
            val merged = group.flatMap(_.selection.selectionSet)
            requireSameNameAndArguments(group) ++ sameForCommonParentsByName(context, parentType, merged)
          }
        })
        parentsCache.put(set, res)
        res
      }

    def doTypesConflict(t1: __Type, t2: __Type): Boolean =
      if (isNonNull(t1))
        if (isNonNull(t2)) (t1.ofType, t2.ofType).mapN((p1, p2) => doTypesConflict(p1, p2)).getOrElse(true)
        else true
      else if (isNonNull(t2))
        true
      else if (isListType(t1))
        if (isListType(t2)) (t1.ofType, t2.ofType).mapN((p1, p2) => doTypesConflict(p1, p2)).getOrElse(true)
        else true
      else if (isListType(t2))
        true
      else if (isLeafType(t1) && isLeafType(t2)) {
        t1.name != t2.name
      } else if (!isComposite(t1) || !isComposite(t2))
        true
      else
        false

    def requireSameNameAndArguments(fields: Set[SelectedField]) =
      cross(fields).flatMap { case (f1, f2) =>
        if (f1.fieldDef.name != f2.fieldDef.name) {
          List(
            s"${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name.getOrElse("")}.${f2.fieldDef.name} are different fields."
          )
        } else if (f1.selection.arguments != f2.selection.arguments)
          List(s"${f1.fieldDef.name} and ${f2.fieldDef.name} have different arguments")
        else List()
      }

    def groupByCommonParents(
      context: Context,
      parentType: __Type,
      fields: Set[SelectedField]
    ): Iterable[Set[SelectedField]] =
      if (groupsCache.get(fields).isDefined) { groupsCache.get(fields).get }
      else {
        val abstractGroup = fields.collect({
          case field if !isConcrete(field.parentType) => field
        })

        val concreteGroups = fields
          .collect({
            case f if isConcrete(f.parentType) && f.parentType.name.isDefined => (f.parentType.name.get, f)
          })
          .foldLeft(Map.empty[String, Set[SelectedField]]) { case (acc, (name, field)) =>
            val value = acc.get(name).map(_ + field).getOrElse(Set(field))
            acc + (name -> value)
          }

        val res =
          if (concreteGroups.size < 1) List(fields)
          else concreteGroups.values.map(_ ++ abstractGroup)

        groupsCache.put(fields, res)
        res
      }

    def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
      IO.fail(ValidationError(msg, explanatoryText))

    val fields = FieldMap(
      context,
      parentType,
      selectionSet
    )

    val conflicts = sameResponseShapeByName(context, parentType, selectionSet) ++
      sameForCommonParentsByName(context, parentType, selectionSet)

    conflicts match {
      case head :: _ =>
        IO.fail(ValidationError(head, ""))
      case _         => IO.unit
    }
  }
}
