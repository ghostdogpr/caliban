package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import zio.{ IO, UIO }
import Utils._
import Utils.syntax._
import Function._
import zio.Ref
import zio.ZIO

object FragmentValidator {
  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] = {
    val fields = FieldMap(
      context,
      parentType,
      selectionSet
    )

    (for {
      shapeCache   <- Ref.make[Map[Iterable[Selection], Iterable[String]]](Map.empty)
      parentsCache <- Ref.make[Map[Iterable[Selection], Iterable[String]]](Map.empty)
      groupsCache  <- Ref.make[Map[Set[SelectedField], Iterable[Set[SelectedField]]]](Map.empty)
      responseShape = sameResponseShapeByName(shapeCache, context, parentType, selectionSet)
      commonParents = sameForCommonParentsByName(parentsCache, groupsCache, context, parentType, selectionSet)
      conflicts    <- responseShape zip commonParents
      all           = conflicts._1 ++ conflicts._2
      _            <- IO.fromOption(all.headOption).flip.mapError(ValidationError(_, ""))
    } yield ())
  }

  def cached[A, R, E, B](cache: Ref[Map[A, B]], a: A)(f: => ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      v   <- cache.get
      res <- v.get(a) match {
               case None    =>
                 for {
                   res <- f
                   _   <- cache.update(_ + (a -> res))
                 } yield res
               case Some(v) => ZIO.succeed(v)
             }
    } yield res

  def sameResponseShapeByName(
    shapeCache: Ref[Map[Iterable[Selection], Iterable[String]]],
    context: Context,
    parentType: __Type,
    set: Iterable[Selection]
  ): UIO[Iterable[String]] =
    cached(shapeCache, set) {
      val fields = FieldMap(context, parentType, set)
      UIO
        .collect(fields.toIterable)({ case (name, values) =>
          UIO
            .collect(cross(values)) { pair =>
              val (f1, f2) = pair
              if (doTypesConflict(f1.fieldDef.`type`(), f2.fieldDef.`type`())) {
                UIO.succeed(
                  List(
                    s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
                      .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
                  )
                )
              } else
                sameResponseShapeByName(
                  shapeCache,
                  context,
                  parentType,
                  f1.selection.selectionSet ++ f2.selection.selectionSet
                )
            }
            .map(_.flatten)
        })
        .map(_.flatten)
    }

  def sameForCommonParentsByName(
    parentsCache: Ref[Map[Iterable[Selection], Iterable[String]]],
    groupsCache: Ref[Map[Set[SelectedField], Iterable[Set[SelectedField]]]],
    context: Context,
    parentType: __Type,
    set: Iterable[Selection]
  ): UIO[Iterable[String]] =
    cached(parentsCache, set) {
      val fields = FieldMap(context, parentType, set)
      UIO
        .collect(fields.toIterable) { case (name, fields) =>
          groupByCommonParents(groupsCache, context, parentType, fields).flatMap { grouped =>
            UIO.collect(grouped) { group =>
              val merged = group.flatMap(_.selection.selectionSet)
              (UIO(requireSameNameAndArguments(group)) <*> sameForCommonParentsByName(
                parentsCache,
                groupsCache,
                context,
                parentType,
                merged
              )).map { case (a, b) => a ++ b }
            }
          }.map(_.flatten)
        }
        .map(_.flatten)
    }
  // }

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
    groupsCache: Ref[Map[Set[SelectedField], Iterable[Set[SelectedField]]]],
    context: Context,
    parentType: __Type,
    fields: Set[SelectedField]
  ): UIO[Iterable[Set[SelectedField]]] =
    cached(groupsCache, fields) {
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

      if (concreteGroups.size < 1) UIO(List(fields))
      else UIO(concreteGroups.values.map(_ ++ abstractGroup))
    }

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))
}
