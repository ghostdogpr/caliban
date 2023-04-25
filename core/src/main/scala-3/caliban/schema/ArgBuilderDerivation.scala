package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.*
import caliban.schema.macros.Macros
import caliban.schema.Annotations.GQLDefault
import caliban.schema.Annotations.GQLName

import scala.deriving.Mirror
import scala.compiletime.*
import scala.util.NotGiven

trait CommonArgBuilderDerivation {
  inline def recurse[Label, A <: Tuple](
    inline values: List[(String, List[Any], ArgBuilder[Any])] = Nil
  ): List[(String, List[Any], ArgBuilder[Any])] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurse[names, ts](
          (
            constValue[name].toString,
            Macros.annotations[t],
            summonInline[ArgBuilder[t]].asInstanceOf[ArgBuilder[Any]]
          ) :: values
        )
    }

  inline def derived[A]: ArgBuilder[A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        lazy val subTypes   = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val traitLabel = constValue[m.MirroredLabel]
        new ArgBuilder[A] {
          def build(input: InputValue): Either[ExecutionError, A] =
            buildSum[A](subTypes, traitLabel)(input)
        }

      case m: Mirror.ProductOf[A] =>
        lazy val fields      = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        lazy val annotations = Macros.paramAnnotations[A].to(Map)
        new ArgBuilder[A] {
          def build(input: InputValue): Either[ExecutionError, A] =
            buildProduct(fields, annotations)(input).map(m.fromProduct)
        }
    }

  private def buildSum[A](
    subTypes: => List[(String, List[Any], ArgBuilder[Any])],
    traitLabel: => String
  )(input: InputValue) =
    (input match {
      case EnumValue(value)   => Some(value)
      case StringValue(value) => Some(value)
      case _                  => None
    }) match {
      case Some(value) =>
        subTypes.find { (label, annotations, _) =>
          label == value || annotations.exists { case GQLName(name) => name == value }
        } match {
          case Some((_, _, builder)) => builder.asInstanceOf[ArgBuilder[A]].build(InputValue.ObjectValue(Map()))
          case None                  => Left(ExecutionError(s"Invalid value $value for trait $traitLabel"))
        }
      case None        => Left(ExecutionError(s"Can't build a trait from input $input"))
    }

  private def buildProduct(
    fields: => List[(String, List[Any], ArgBuilder[Any])],
    annotations: => Map[String, List[Any]]
  )(input: InputValue) =
    fields.map { (label, _, builder) =>
      input match {
        case InputValue.ObjectValue(fields) =>
          val finalLabel =
            annotations.getOrElse(label, Nil).collectFirst { case GQLName(name) => name }.getOrElse(label)
          val default    = annotations.getOrElse(label, Nil).collectFirst { case GQLDefault(v) => v }
          fields.get(finalLabel).fold(builder.buildMissing(default))(builder.build)
        case value                          => builder.build(value)
      }
    }.foldRight[Either[ExecutionError, Tuple]](Right(EmptyTuple)) { case (item, acc) =>
      item match {
        case error: Left[ExecutionError, Any] => error.asInstanceOf[Left[ExecutionError, Tuple]]
        case Right(value)                     => acc.map(value *: _)
      }
    }
}

trait ArgBuilderDerivation extends CommonArgBuilderDerivation {
  inline def gen[A]: ArgBuilder[A] = derived

  sealed trait Auto[A] extends ArgBuilder[A] {
    inline given genAuto[T](using NotGiven[ArgBuilder[T]]): ArgBuilder[T] = derived
  }

  object Auto {
    inline def derived[A]: Auto[A] = new {
      private val impl = ArgBuilder.gen[A]
      export impl.*
    }
  }
}

trait AutoArgBuilderDerivation extends ArgBuilderInstances with LowPriorityDerivedArgBuilder

private[schema] trait LowPriorityDerivedArgBuilder extends CommonArgBuilderDerivation {
  inline implicit def genAuto[A]: ArgBuilder[A] = derived
}
