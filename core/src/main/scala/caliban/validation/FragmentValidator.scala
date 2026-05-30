package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import caliban.validation.Utils._
import zio.Chunk

import scala.collection.mutable

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): Either[ValidationError, Unit] = {

    val shapeCache   = mutable.HashMap.empty[List[Selection], Chunk[String]]
    val parentsCache = mutable.HashMap.empty[Iterable[Selection], Chunk[String]]
    val groupsCache  = mutable.HashMap.empty[Set[SelectedField], Chunk[Set[SelectedField]]]

    def sameResponseShapeByName(set: List[Selection], parentType: __Type): Chunk[String] =
      if (set.isEmpty) Chunk.empty
      else
        shapeCache.getOrElseUpdate(
          set, {
            val fields = FieldMap.make(context, parentType, set)
            Chunk.fromIterable(fields.flatMap { case (name, values) =>
              cross(values, includeIdentity = true).flatMap { case (f1, f2) =>
                if (doTypesConflict(f1.fieldDef._type, f2.fieldDef._type)) {
                  Chunk.single(
                    s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                        .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
                  )
                } else
                  sameResponseShapeByName(f1.selection.selectionSet ::: f2.selection.selectionSet, f1.fieldDef._type)
              }
            })
          }
        )

    def sameForCommonParentsByName(set: Iterable[Selection]): Chunk[String] =
      if (set.isEmpty) Chunk.empty
      else
        parentsCache.getOrElseUpdate(
          set, {
            val fields = FieldMap.make(context, parentType, set)
            Chunk.fromIterable(fields.values.flatMap { fields =>
              groupByCommonParents(fields).flatMap { group =>
                val merged = group.flatMap(_.selection.selectionSet)
                requireSameNameAndArguments(group) ++ sameForCommonParentsByName(merged)
              }
            })
          }
        )

    def doTypesConflict(t1: __Type, t2: __Type): Boolean =
      if (isNonNull(t1))
        if (isNonNull(t2)) t1.ofType.flatMap(p1 => t2.ofType.map(p2 => doTypesConflict(p1, p2))).getOrElse(true)
        else true
      else if (isNonNull(t2))
        true
      else if (isListType(t1))
        if (isListType(t2)) t1.ofType.flatMap(p1 => t2.ofType.map(p2 => doTypesConflict(p1, p2))).getOrElse(true)
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
      cross(fields, includeIdentity = false).flatMap { case (f1, f2) =>
        if (f1.fieldDef.name != f2.fieldDef.name) {
          Some(
            s"${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name.getOrElse("")}.${f2.fieldDef.name} are different fields."
          )
        } else if (f1.selection.arguments != f2.selection.arguments)
          Some(s"${f1.fieldDef.name} and ${f2.fieldDef.name} have different arguments")
        else None
      }

    def groupByCommonParents(fields: Set[SelectedField]): Chunk[Set[SelectedField]] =
      groupsCache.getOrElseUpdate(
        fields, {
          val abstractGroup = fields.filter(field => isAbstract(field.parentType))

          val concreteGroups =
            mutable.HashMap.empty[String, mutable.Builder[SelectedField, Set[SelectedField]]]

          fields.foreach {
            case field @ SelectedField(
                  __Type(_, Some(name), _, _, _, _, _, _, _, _, _, _, _),
                  _,
                  _
                ) if isConcrete(field.parentType) =>
              concreteGroups.getOrElseUpdate(name, Set.newBuilder ++= abstractGroup) += field
            case _ => ()
          }

          if (concreteGroups.isEmpty) Chunk.single(fields)
          else Chunk.fromIterable(concreteGroups.values.map(_.result()))
        }
      )

    val conflicts = sameResponseShapeByName(selectionSet, parentType) ++ sameForCommonParentsByName(selectionSet)
    if (conflicts.nonEmpty) {
      Left(ValidationError(conflicts.head, ""))
    } else {
      ValidationOps.unit
    }
  }
}
