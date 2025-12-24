package caliban.schema

import caliban.InputValue
import caliban.Value.*
import caliban.schema.Annotations.GQLOneOfInput
import caliban.schema.macros.Macros
import magnolia1.Macro as MagnoliaMacro

import scala.annotation.nowarn
import scala.compiletime.*
import scala.deriving.Mirror
import scala.util.NotGiven

trait CommonArgBuilderDerivation {

  transparent inline def recurseSum[P, Label <: Tuple, A <: Tuple](
    inline values: List[(String, List[Any], ArgBuilder[Any])] = Nil
  ): List[(String, List[Any], ArgBuilder[Any])] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurseSum[P, names, ts] {
          inline summonInline[Mirror.Of[t]] match {
            case m: Mirror.SumOf[t] =>
              recurseSum[t, m.MirroredElemLabels, m.MirroredElemTypes](values)
            case _                  =>
              (
                constValue[name].toString,
                MagnoliaMacro.anns[t], {
                  inline if (Macros.isEnumField[P, t])
                    inline if (!Macros.implicitExists[ArgBuilder[t]]) derived[t]
                    else summonInline[ArgBuilder[t]]
                  else summonInline[ArgBuilder[t]]
                }.asInstanceOf[ArgBuilder[Any]]
              ) :: values
          }
        }
    }

  transparent inline def recurseProduct[P, Label <: Tuple, A <: Tuple](
    inline values: List[(String, ArgBuilder[Any])] = Nil
  ): List[(String, ArgBuilder[Any])] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurseProduct[P, names, ts](
          (
            constValue[name].toString,
            summonInline[ArgBuilder[t]].asInstanceOf[ArgBuilder[Any]]
          ) :: values
        )
    }

  inline def derived[A]: ArgBuilder[A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        inline if (Macros.hasAnnotation[A, GQLOneOfInput]) {
          new OneOfArgBuilder[A](
            recurseSum[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )
        } else
          new SumArgBuilder[A](
            recurseSum[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )

      case m: Mirror.ProductOf[A] =>
        inline if (
          !DerivationUtils.hasSingleField[m.MirroredElemTypes]
          && DerivationUtils.isValueType[A, m.MirroredElemLabels]
        ) {
          scala.compiletime.error("value classes must have exactly one field")
        } else
          new ProductArgBuilder(
            recurseProduct[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            MagnoliaMacro.paramAnns[A].toMap,
            DerivationUtils.isValueType[A, m.MirroredElemLabels]
          )(m.fromProduct)
    }
}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  inline def gen[A]: ArgBuilder[A] = derived

  sealed abstract class Auto[A] extends ArgBuilder[A] {
    inline given genAuto[T](using NotGiven[ArgBuilder[T]]): ArgBuilder[T] = derived
  }

  object Auto {
    @nowarn("msg=anonymous class")
    inline def derived[A]: Auto[A] = new {
      private val impl = ArgBuilder.derived[A]
      export impl.*
    }
  }

  /**
   * Due to a Scala 3 compiler bug, it's not possible to derive two type classes of the same name. For example, the following fails to compile:
   *
   * `case class Foo(value: String) derives Schema.Auto, ArgBuilder.Auto`
   *
   * Until the issue is resolved, we can use this type alias as a workaround by replacing `ArgBuilder.Auto` with `ArgBuilder.GenAuto`
   */
  final type GenAuto[A] = Auto[A]

}

trait AutoArgBuilderDerivation extends ArgBuilderInstances with LowPriorityDerivedArgBuilder

private[schema] trait LowPriorityDerivedArgBuilder extends CommonArgBuilderDerivation {
  inline implicit def genAuto[A]: ArgBuilder[A] = derived
}
