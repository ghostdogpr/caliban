package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import zio.{ Chunk, IO, UIO }
import Utils._
import Utils.syntax._
import scala.collection.mutable

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] = {
    val shapeCache   = scala.collection.mutable.Map.empty[Iterable[Selection], Chunk[String]]
    val parentsCache = scala.collection.mutable.Map.empty[Iterable[Selection], Chunk[String]]
    val groupsCache  = scala.collection.mutable.Map.empty[Set[SelectedField], Chunk[Set[SelectedField]]]

    def sameResponseShapeByName(context: Context, parentType: __Type, set: Iterable[Selection]): Chunk[String] =
      shapeCache.get(set) match {
        case Some(value) => value
        case None        =>
          val fields = FieldMap(context, parentType, set)
          val res    = Chunk.fromIterable(fields.flatMap { case (name, values) =>
            cross(values).flatMap { case (f1, f2) =>
              if (doTypesConflict(f1.fieldDef.`type`(), f2.fieldDef.`type`())) {
                Chunk(
                  s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                    .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
                )
              } else
                sameResponseShapeByName(context, parentType, f1.selection.selectionSet ++ f2.selection.selectionSet)
            }
          })
          shapeCache.put(set, res)
          res
      }

    def sameForCommonParentsByName(context: Context, parentType: __Type, set: Iterable[Selection]): Chunk[String] =
      parentsCache.get(set) match {
        case Some(value) => value
        case None        =>
          val fields = FieldMap(context, parentType, set)
          val res    = Chunk.fromIterable(fields.flatMap({ case (name, fields) =>
            groupByCommonParents(context, parentType, fields).flatMap { group =>
              val merged = group.flatMap(_.selection.selectionSet)
              requireSameNameAndArguments(group) ++ sameForCommonParentsByName(context, parentType, merged)
            }
          }))
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
    ): Chunk[Set[SelectedField]] =
      groupsCache.get(fields) match {
        case Some(value) => value
        case None        =>
          val abstractGroup = fields.collect {
            case field if !isConcrete(field.parentType) => field
          }

          val concreteGroups = mutable.Map.empty[String, Set[SelectedField]]

          fields
            .foreach({
              case field @ SelectedField(
                    __Type(_, Some(name), _, _, _, _, _, _, _, _, _),
                    _,
                    _
                  ) if isConcrete(field.parentType) =>
                val value = concreteGroups.get(name).map(_ + field).getOrElse(Set(field))
                concreteGroups.put(name, value)
              case _ => ()
            })

          val res =
            if (concreteGroups.size < 1) Chunk(fields)
            else Chunk.fromIterable(concreteGroups.values.map(_ ++ abstractGroup))

          groupsCache.put(fields, res)
          res
      }

    val fields = FieldMap(
      context,
      parentType,
      selectionSet
    )

    val conflicts = sameResponseShapeByName(context, parentType, selectionSet) ++
      sameForCommonParentsByName(context, parentType, selectionSet)

    IO.whenCase(conflicts) { case Chunk(head, _*) =>
      IO.fail(ValidationError(head, ""))
    }
  }
}
