package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.{ schema, CalibanError, InputValue }
import caliban.Value.*
import caliban.schema.macros.Macros
import caliban.schema.Annotations.{ GQLDefault, GQLName, GQLOneOfInputName, GQLValueType }

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
            Macros.annotations[t], {
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
          Macros.paramAnnotations[A].to(Map)
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

    private lazy val builders: Map[String, (ArgBuilder[A], Boolean)] =
      _subTypes.map { (label, annotations, argBuilder) =>
        val name        = Types.oneOfInputFieldName(label, annotations)
        val isValueType = annotations.exists { case GQLValueType(_) => true; case _ => false }
        name -> (argBuilder.asInstanceOf[ArgBuilder[A]], isValueType)
      }.toMap

    private lazy val traitLabel = _traitLabel

    private def error(input: InputValue) = ExecutionError(s"Invalid oneOf input $input for trait $traitLabel")

    def build(input: InputValue): Either[ExecutionError, A] = input match {
      case InputValue.ObjectValue(fields) =>
        fields.toList match {
          case (key, innerValue) :: Nil =>
            builders.get(key).toRight(error(input)).flatMap { (builder, isValueType) =>
              innerValue match {
                case v: InputValue.ObjectValue if !isValueType => builder.build(v)
                case v if isValueType                          => builder.build(v)
                case _                                         => Left(error(input))
              }
            }
          case _                        =>
            Left(ExecutionError("Exactly one key must be specified for oneOf inputs"))
        }
      case _                              => Left(error(input))
    }
  }

  private def makeProductArgBuilder[A](
    _fields: => List[(String, List[Any], ArgBuilder[Any])],
    _annotations: => Map[String, List[Any]]
  )(fromProduct: Product => A): ArgBuilder[A] = new ArgBuilder[A] {
    private lazy val fields      = _fields
    private lazy val annotations = _annotations

    def build(input: InputValue): Either[ExecutionError, A] =
      fields.view.map { (label, _, builder) =>
        input match {
          case InputValue.ObjectValue(fields) =>
            val labelList  = annotations.get(label)
            def default    = labelList.flatMap(_.collectFirst { case GQLDefault(v) => v })
            val finalLabel = labelList.flatMap(_.collectFirst { case GQLName(name) => name }).getOrElse(label)
            fields.get(finalLabel).fold(builder.buildMissing(default))(builder.build)
          case value                          => builder.build(value)
        }
      }.foldLeft[Either[ExecutionError, Tuple]](Right(EmptyTuple)) { case (acc, item) =>
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
