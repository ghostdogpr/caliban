package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.{ schema, CalibanError, InputValue }
import caliban.Value.*
import caliban.schema.Annotations.{ GQLDefault, GQLExcluded, GQLName }
import caliban.schema.macros.Macros
import magnolia1.Macro as MagnoliaMacro

import scala.deriving.Mirror
import scala.compiletime.*
import scala.util.NotGiven

trait CommonArgBuilderDerivation {
  inline def recurse[P, Label, A <: Tuple](
    inline values: List[(String, List[Any], ArgBuilder[Any])] = Nil
  ): List[(String, List[Any], ArgBuilder[Any])] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurse[P, names, ts](
          (
            constValue[name].toString,
            MagnoliaMacro.anns[t], {
              if (Macros.isEnumField[P, t])
                if (!Macros.implicitExists[ArgBuilder[t]]) derived[t]
                else summonInline[ArgBuilder[t]]
              else summonInline[ArgBuilder[t]]
            }.asInstanceOf[ArgBuilder[Any]]
          ) :: values
        )
    }

  inline def derived[A]: ArgBuilder[A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        inline if (Macros.hasOneOfInputAnnotation[A]) {
          makeOneOfBuilder[A](
            recurse[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )
        } else
          makeSumArgBuilder[A](
            recurse[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )

      case m: Mirror.ProductOf[A] =>
        makeProductArgBuilder(
          recurse[A, m.MirroredElemLabels, m.MirroredElemTypes](),
          MagnoliaMacro.paramAnns[A].to(Map)
        )(m.fromProduct)
    }

  private def makeSumArgBuilder[A](
    _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
    _traitLabel: => String
  ): ArgBuilder[A] = new ArgBuilder[A] {
    private lazy val subTypes   = _subTypes
    private lazy val traitLabel = _traitLabel
    private val emptyInput      = InputValue.ObjectValue(Map())

    def build(input: InputValue): Either[ExecutionError, A] =
      input.match {
        case EnumValue(value)   => Right(value)
        case StringValue(value) => Right(value)
        case _                  => Left(ExecutionError(s"Can't build a trait from input $input"))
      }.flatMap { value =>
        subTypes.collectFirst {
          case (
                label,
                annotations,
                builder: ArgBuilder[A @unchecked]
              ) if label == value || annotations.exists { case GQLName(name) => name == value } =>
            builder
        }
          .toRight(ExecutionError(s"Invalid value $value for trait $traitLabel"))
          .flatMap(_.build(emptyInput))
      }
  }

  private def makeOneOfBuilder[A](
    _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
    _traitLabel: => String
  ): ArgBuilder[A] = new ArgBuilder[A] {
    private lazy val traitLabel = _traitLabel

    private lazy val combined: ArgBuilder[A] =
      _subTypes.map(_._3).asInstanceOf[List[ArgBuilder[A]]] match {
        case head :: tail =>
          tail.foldLeft(head)(_ orElse _).orElse((input: InputValue) => Left(inputError(input)))
        case _            =>
          (_: InputValue) => Left(ExecutionError("OneOf Input Objects must have at least one subtype"))
      }

    private def inputError(input: InputValue) =
      ExecutionError(s"Invalid oneOf input $input for trait $traitLabel")

    def build(input: InputValue): Either[ExecutionError, A] = input match {
      case InputValue.ObjectValue(f) if f.size == 1 => combined.build(input)
      case InputValue.ObjectValue(_)                => Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
      case _                                        => Left(inputError(input))
    }
  }

  private def makeProductArgBuilder[A](
    _fields: => List[(String, List[Any], ArgBuilder[Any])],
    _annotations: => Map[String, List[Any]]
  )(fromProduct: Product => A): ArgBuilder[A] = new ArgBuilder[A] {

    private lazy val annotations = _annotations

    private lazy val fields = _fields.map { (label, _, builder) =>
      val labelList  = annotations.get(label)
      val default    = labelList.flatMap(_.collectFirst { case GQLDefault(v) => v })
      val finalLabel = labelList.flatMap(_.collectFirst { case GQLName(name) => name }).getOrElse(label)
      (finalLabel, default, builder)
    }

    def build(input: InputValue): Either[ExecutionError, A] =
      fields.view.map { (label, default, builder) =>
        input match {
          case InputValue.ObjectValue(fields) => fields.get(label).fold(builder.buildMissing(default))(builder.build)
          case value                          => builder.build(value)
        }
      }.foldLeft[Either[ExecutionError, Tuple]](Right(EmptyTuple)) { (acc, item) =>
        item match {
          case Right(value) => acc.map(_ :* value)
          case Left(e)      => Left(e)
        }
      }.map(fromProduct)
  }
}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  inline def gen[A]: ArgBuilder[A] = derived

  sealed trait Auto[A] extends ArgBuilder[A] {
    inline given genAuto[T](using NotGiven[ArgBuilder[T]]): ArgBuilder[T] = derived
  }

  object Auto {
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
