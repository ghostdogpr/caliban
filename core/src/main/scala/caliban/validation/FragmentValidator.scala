package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import caliban.validation.Utils._
import zio.Chunk

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): Either[ValidationError, Unit] = {

    // NOTE: We use the `hashCode()` as the key since it's much more performant
    val shapeCache   = mutable.Map.empty[Int, Chunk[String]]
    val parentsCache = mutable.Map.empty[Int, Chunk[String]]
    val groupsCache  = mutable.Map.empty[Int, Chunk[Set[SelectedField]]]

    def sameResponseShapeByName(set: Iterable[Selection], parentType: __Type): Chunk[String] = {
      val keyHash = MurmurHash3.unorderedHash(set)
      shapeCache.get(keyHash) match {
        case Some(value) => value
        case None        =>
          val fields = FieldMap(context, parentType, set)
          val res    = Chunk.fromIterable(fields.flatMap { case (name, values) =>
            cross(values, includeIdentity = true).flatMap { case (f1, f2) =>
              if (doTypesConflict(f1.fieldDef._type, f2.fieldDef._type)) {
                Chunk(
                  s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                      .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
                )
              } else
                sameResponseShapeByName(f1.selection.selectionSet ::: f2.selection.selectionSet, f1.fieldDef._type)
            }
          })
          shapeCache.update(keyHash, res)
          res
      }
    }

    def sameForCommonParentsByName(set: Iterable[Selection]): Chunk[String] = {
      val keyHash = MurmurHash3.unorderedHash(set)
      parentsCache.get(keyHash) match {
        case Some(value) => value
        case None        =>
          val fields = FieldMap(context, parentType, set)
          val res    = Chunk.fromIterable(fields.flatMap { case (_, fields) =>
            groupByCommonParents(fields).flatMap { group =>
              val merged = group.flatMap(_.selection.selectionSet)
              requireSameNameAndArguments(group) ++ sameForCommonParentsByName(merged)
            }
          })
          parentsCache.update(keyHash, res)
          res
      }
    }

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

    def groupByCommonParents(fields: Set[SelectedField]): Chunk[Set[SelectedField]] = {
      val keyHash = fields.hashCode()
      groupsCache.get(keyHash) match {
        case Some(value) => value
        case None        =>
          val abstractGroup = fields.collect {
            case field if !isConcrete(field.parentType) => field
          }

          val concreteGroups =
            mutable.Map.empty[String, mutable.Builder[SelectedField, Set[SelectedField]]]

          fields.foreach {
            case field @ SelectedField(
                  __Type(_, Some(name), _, _, _, _, _, _, _, _, _, _, _),
                  _,
                  _
                ) if isConcrete(field.parentType) =>
              concreteGroups.getOrElseUpdate(name, Set.newBuilder ++= abstractGroup) += field
            case _ => ()
          }

          val res =
            if (concreteGroups.isEmpty) Chunk(fields)
            else Chunk.fromIterable(concreteGroups.values.map(_.result()))

          groupsCache.update(keyHash, res)
          res
      }
    }

    val conflicts = sameResponseShapeByName(selectionSet, parentType) ++ sameForCommonParentsByName(selectionSet)
    if (conflicts.nonEmpty) {
      Left(ValidationError(conflicts.head, ""))
    } else {
      ValidationOps.unit
    }
  }
}
