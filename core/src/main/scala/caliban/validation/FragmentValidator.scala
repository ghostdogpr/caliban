package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import caliban.validation.Utils._
import caliban.validation.Utils.syntax._
import zio.Chunk
import zio.prelude.EReader
import zio.prelude.fx.ZPure

import scala.collection.mutable

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): EReader[Any, ValidationError, Unit] = {

    // NOTE: We use the `hashCode()` as the key since it's much more performant
    val shapeCache   = mutable.Map.empty[Int, Chunk[String]]
    val parentsCache = mutable.Map.empty[Int, Chunk[String]]
    val groupsCache  = mutable.Map.empty[Int, Chunk[Set[SelectedField]]]

    def sameResponseShapeByName(set: Iterable[Selection]): Chunk[String] = {
      val keyHash = set.hashCode()
      shapeCache.get(keyHash) match {
        case Some(value) => value
        case None        =>
          val fields = FieldMap(context, parentType, set)
          val res    = Chunk.fromIterable(fields.flatMap { case (name, values) =>
            cross(values).flatMap { case (f1, f2) =>
              if (doTypesConflict(f1.fieldDef._type, f2.fieldDef._type)) {
                Chunk(
                  s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                      .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
                )
              } else
                sameResponseShapeByName(f1.selection.selectionSet ::: f2.selection.selectionSet)
            }
          })
          shapeCache.update(keyHash, res)
          res
      }
    }

    def sameForCommonParentsByName(set: Iterable[Selection]): Chunk[String] = {
      val keyHash = set.hashCode()
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
                  __Type(_, Some(name), _, _, _, _, _, _, _, _, _, _),
                  _,
                  _
                ) if isConcrete(field.parentType) =>
              concreteGroups.get(name) match {
                case Some(v) => v += field
                case None    =>
                  val sb = Set.newBuilder ++= abstractGroup
                  sb += field
                  concreteGroups.update(name, sb)
              }
            case _ => ()
          }

          val res =
            if (concreteGroups.isEmpty) Chunk(fields)
            else Chunk.fromIterable(concreteGroups.values.map(_.result()))

          groupsCache.update(keyHash, res)
          res
      }
    }

    val conflicts = sameResponseShapeByName(selectionSet) ++ sameForCommonParentsByName(selectionSet)
    conflicts match {
      case Chunk(head, _*) => ZPure.fail(ValidationError(head, ""))
      case _               => ZPure.unit
    }
  }
}
