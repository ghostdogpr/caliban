package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue.{ ListValue, VariableValue }
import caliban.Value.*
import caliban.schema.Annotations.{ GQLDefault, GQLName, GQLOneOfInput }
import caliban.schema.macros.Macros
import caliban.{ CalibanError, InputValue }
import magnolia1.Macro as MagnoliaMacro

import scala.compiletime.*
import scala.deriving.Mirror
import scala.util.NotGiven

trait CommonArgBuilderDerivation {
  import caliban.syntax.*

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
          makeOneOfBuilder[A](
            recurseSum[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )
        } else
          makeSumArgBuilder[A](
            recurseSum[A, m.MirroredElemLabels, m.MirroredElemTypes](),
            constValue[m.MirroredLabel]
          )

      case m: Mirror.ProductOf[A] =>
        makeProductArgBuilder(
          recurseProduct[A, m.MirroredElemLabels, m.MirroredElemTypes](),
          MagnoliaMacro.paramAnns[A].toMap
        )(m.fromProduct)
    }

  private def makeSumArgBuilder[A](
    _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
    traitLabel: String
  ): ArgBuilder[A] = new ArgBuilder[A] {
    private lazy val subTypes = _subTypes
    private val emptyInput    = InputValue.ObjectValue(Map.empty)

    final def build(input: InputValue): Either[ExecutionError, A] =
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
              ) if label == value || annotations.contains(GQLName(value)) =>
            builder
        }
          .toRight(ExecutionError(s"Invalid value $value for trait $traitLabel"))
          .flatMap(_.build(emptyInput))
      }
  }

  private def makeOneOfBuilder[A](
    _subTypes: => List[(String, List[Any], ArgBuilder[Any])],
    traitLabel: String
  ): ArgBuilder[A] = new ArgBuilder[A] {

    override val partial: PartialFunction[InputValue, Either[ExecutionError, A]] = {
      val xs = _subTypes.map(_._3).asInstanceOf[List[ArgBuilder[A]]]

      val checkSize: PartialFunction[InputValue, Either[ExecutionError, A]] = {
        case InputValue.ObjectValue(f) if f.size != 1 =>
          Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
      }
      xs.foldLeft(checkSize)(_ orElse _.partial)
    }

    def build(input: InputValue): Either[ExecutionError, A] =
      partial.applyOrElse(input, (in: InputValue) => Left(inputError(in)))

    private def inputError(input: InputValue) =
      ExecutionError(s"Invalid oneOf input $input for trait $traitLabel")
  }

  private def makeProductArgBuilder[A](
    _fields: => List[(String, ArgBuilder[Any])],
    annotations: Map[String, List[Any]]
  )(fromProduct: Product => A): ArgBuilder[A] = new ArgBuilder[A] {

    private val params = Array.from(_fields.map { (label, builder) =>
      val labelList  = annotations.get(label)
      val default    = builder.buildMissing(labelList.flatMap(_.collectFirst { case GQLDefault(v) => v }))
      val finalLabel = labelList.flatMap(_.collectFirst { case GQLName(name) => name }).getOrElse(label)
      (finalLabel, default, builder)
    })

    private val required = params.collect { case (label, default, _) if default.isLeft => label }

    override private[schema] val partial: PartialFunction[InputValue, Either[ExecutionError, A]] = {
      case InputValue.ObjectValue(fields) if required.forall(fields.contains) => fromFields(fields)
    }

    def build(input: InputValue): Either[ExecutionError, A] =
      input match {
        case InputValue.ObjectValue(fields) => fromFields(fields)
        case value                          => fromValue(value)
      }

    private def fromFields(fields: Map[String, InputValue]): Either[ExecutionError, A] = {
      var i   = 0
      val l   = params.length
      val arr = Array.ofDim[Any](l)
      while (i < l) {
        val (label, default, builder) = params(i)
        val field                     = fields.getOrElseNull(label)
        val value                     = if (field ne null) builder.build(field) else default
        value match {
          case Right(v) => arr(i) = v
          case Left(e)  => return Left(e)
        }
        i += 1
      }
      Right(fromProduct(Tuple.fromArray(arr)))
    }

    private def fromValue(input: InputValue): Either[ExecutionError, A] = {
      val l   = params.length
      val arr = Array.ofDim[Any](l)
      var i   = 0
      while (i < l) {
        val (_, _, builder) = params(i)
        builder.build(input) match {
          case Right(v) => arr(i) = v
          case Left(e)  => return Left(e)
        }
        i += 1
      }
      Right(fromProduct(Tuple.fromArray(arr)))
    }

  }
}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  inline def gen[A]: ArgBuilder[A] = derived

  sealed abstract class Auto[A] extends ArgBuilder[A] {
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
