package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import zio.{ IO, UIO }
import Utils._

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
      sameResponseShapeByName_    <- IO.memoize((sameResponseShapeByName _).tupled).map(f => Function.untupled(f))
      groupByCommonParents_       <- IO.memoize((groupByCommonParents _).tupled).map(f => Function.untupled(f))
      sameForCommonParentsByName_ <-
        IO.memoize((sameForCommonParentsByName(groupByCommonParents_) _).tupled).map(f => Function.untupled(f))
      responseShape                = sameResponseShapeByName_(context, parentType, fields)
      commonParents                = sameForCommonParentsByName_(context, parentType, fields)
      conflicts                   <- responseShape zip commonParents
      all                          = conflicts._1 ++ conflicts._2
      _                           <- IO.fromOption(all.headOption).flip.mapError(ValidationError(_, ""))
    } yield ())
  }

  def sameResponseShapeByName(context: Context, parentType: __Type, fields: FieldMap): UIO[Iterable[String]] =
    UIO.succeed(fields.flatMap { case (name, values) =>
      cross(values).flatMap { pair =>
        val (f1, f2) = pair
        if (doTypesConflict(f1.fieldDef.`type`(), f2.fieldDef.`type`()))
          List(
            s"$name has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
              .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
          )
        else List()
      }
    })

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
    else if (isLeafType(t1) && isLeafType(t2))
      t1.name != t2.name
    else if (!isComposite(t1) || !isComposite(t2))
      true
    else
      false

  def sameForCommonParentsByName(
    groupByCommonParents: (Context, __Type, Set[SelectedField]) => UIO[List[Set[SelectedField]]]
  )(context: Context, parentType: __Type, fields: FieldMap): UIO[Iterable[String]] =
    UIO
      .collect(fields.toIterable)({ case (_, fields) =>
        groupByCommonParents(context, parentType, fields).map { grouped =>
          grouped.flatMap(group => requireSameNameAndArguments(group))
        }
      })
      .map(_.flatten)

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
  ): UIO[List[Set[SelectedField]]] = {
    val abstractGroup = fields.collect({
      case field if !isConcrete(field.parentType) => field
    })

    val concreteGroups = fields
      .collect({
        case f if isConcrete(f.parentType) && f.parentType.name.isDefined => (f.parentType.name.get, f)
      })
      .foldLeft(Map.empty[String, Set[SelectedField]])({ case (acc, (name, field)) =>
        val entry = acc.get(name).map(_ + field).getOrElse(Set(field))
        acc + (name -> entry)
      })

    if (concreteGroups.size < 1) UIO.succeed(List(fields))
    else UIO.succeed(concreteGroups.map({ case (_, v) => v ++ abstractGroup }).toList)
  }

  implicit class OptionSyntax[+A](val self: Option[A]) extends AnyVal {
    def zip[B](that: Option[B]): Option[(A, B)] =
      self.flatMap(a => that.map(b => (a, b)))
  }

  implicit class Tuple2Syntax[+A, +B](val self: Tuple2[Option[A], Option[B]]) extends AnyVal {
    def mapN[C](f: (A, B) => C): Option[C] =
      self._1.flatMap(a => self._2.map(b => f(a, b)))
  }

  def cross[A](a: Iterable[A]): Iterable[(A, A)]                                      =
    for (xs <- a; ys <- a) yield (xs, ys)

  def cross[A](a: Iterable[A], b: Iterable[A]): Iterable[(A, A)]                      =
    for (xs <- a; ys <- b) yield (xs, ys)

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))
}
