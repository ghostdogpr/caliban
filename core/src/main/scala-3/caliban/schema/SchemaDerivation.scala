package caliban.schema

import caliban.introspection.adt.*
import caliban.schema.Annotations.*
import caliban.schema.macros.Macros
import magnolia1.Macro as MagnoliaMacro

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

object PrintDerived {
  import scala.quoted.*
  inline def apply[T](inline any: T): T                                = ${ printDerived('any) }
  def printDerived[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect.*
    println(Printer.TreeShortCode.show(any.asTerm))
    any
  }
}

trait CommonSchemaDerivation {
  export DerivationUtils.customizeInputTypeName

  case class DerivationConfig(
    /**
     * Whether to enable the `SemanticNonNull` feature on derivation.
     * It is currently disabled by default since it is not yet stable.
     */
    enableSemanticNonNull: Boolean = false
  )

  /**
   * Returns a configuration object that can be used to customize the derivation behavior.
   *
   * Override this method to customize the configuration.
   */
  def config: DerivationConfig = DerivationConfig()

  inline def recurseSum[R, P, Label, A <: Tuple](
    inline types: List[(String, __Type, List[Any])] = Nil,
    inline schemas: List[Schema[R, Any]] = Nil
  ): (
    List[(String, __Type, List[Any])],
    List[Schema[R, Any]]
  ) =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => (types, schemas.reverse)
      case (_: (name *: names), _: (t *: ts)) =>
        val schema = {
          inline if (Macros.isEnumField[P, t])
            inline if (!Macros.implicitExists[Schema[R, t]]) derived[R, t]
            else summonInline[Schema[R, t]]
          else summonInline[Schema[R, t]]
        }.asInstanceOf[Schema[R, Any]]

        recurseSum[R, P, names, ts](
          types = inline summonInline[Mirror.Of[t]] match {
            case m: Mirror.SumOf[t] =>
              recurseSum[R, t, m.MirroredElemLabels, m.MirroredElemTypes](types)._1
            case _                  =>
              (
                constValue[name].toString,
                schema.toType_(),
                // Workaround until we figure out why the macro uses the parent's annotations when the leaf is a Scala 3 enum
                inline if (!MagnoliaMacro.isEnum[t]) MagnoliaMacro.anns[t] else Nil
              ) :: types
          },
          schemas = schema :: schemas
        )

    }

  inline def recurseProduct[R, P, Label, A <: Tuple](
    inline values: List[(String, Schema[R, Any], Int)] = Nil
  )(inline index: Int = 0): List[(String, Schema[R, Any], Int)] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _)                 => values.reverse
      case (_: (name *: names), _: (t *: ts)) =>
        recurseProduct[R, P, names, ts] {
          inline if (Macros.isFieldExcluded[P, name]) values
          else
            (
              constValue[name].toString,
              summonInline[Schema[R, t]].asInstanceOf[Schema[R, Any]],
              index
            ) :: values
        }(index + 1)
    }

  inline def valueTypeSchema[R, Label, A <: Tuple]: Schema[R, Any] =
    inline erasedValue[(Label, A)] match {
      case (_: EmptyTuple, _) => error("GQLValueType case classes must have at least one field")
      case (_, _: (t *: _))   => summonInline[Schema[R, t]].asInstanceOf[Schema[R, Any]]
    }

  inline def derived[R, A]: Schema[R, A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        new SumSchema[R, A](
          recurseSum[R, A, m.MirroredElemLabels, m.MirroredElemTypes](),
          MagnoliaMacro.typeInfo[A],
          MagnoliaMacro.anns[A]
        )(m.ordinal)

      case m: Mirror.ProductOf[A] =>
        inline erasedValue[m.MirroredElemLabels] match {
          case _: EmptyTuple                              =>
            new EnumValueSchema[R, A](
              MagnoliaMacro.typeInfo[A],
              // Workaround until we figure out why the macro uses the parent's annotations when the leaf is a Scala 3 enum
              inline if (!MagnoliaMacro.isEnum[A]) MagnoliaMacro.anns[A] else Nil,
              config.enableSemanticNonNull
            )
          case _ if Macros.hasAnnotation[A, GQLValueType] =>
            new ValueTypeSchema[R, A](
              valueTypeSchema[R, m.MirroredElemLabels, m.MirroredElemTypes],
              MagnoliaMacro.typeInfo[A],
              MagnoliaMacro.anns[A]
            )
          case _                                          =>
            new ObjectSchema[R, A](
              recurseProduct[R, A, m.MirroredElemLabels, m.MirroredElemTypes]()(),
              Macros.fieldsFromMethods[R, A],
              MagnoliaMacro.typeInfo[A],
              MagnoliaMacro.anns[A],
              MagnoliaMacro.paramAnns[A].toMap,
              config.enableSemanticNonNull
            )(using summonInline[ClassTag[A]])
        }

    }
}

trait SchemaDerivation[R] extends CommonSchemaDerivation {
  inline def apply[A]: Schema[R, A] = summonInline[Schema[R, A]]

  inline def gen[R, A]: Schema[R, A] = derived[R, A]

  inline def genDebug[R, A]: Schema[R, A] = PrintDerived(derived[R, A])

  final lazy val auto = new AutoSchemaDerivation[Any] {}

  final class SemiAuto[A](impl: Schema[R, A]) extends Schema[R, A] {
    export impl.*
  }

  object SemiAuto {
    inline def derived[A]: SemiAuto[A] = new SemiAuto[A](Schema.derived[R, A])
  }

  sealed abstract class Auto[A] extends Schema[R, A] {
    inline given genAuto[T](using NotGiven[Schema[R, T]]): Schema[R, T] = derived[R, T]
  }

  object Auto {
    inline def derived[A]: Auto[A] = new {
      private val impl = Schema.derived[R, A]
      export impl.*
    }
  }
}

trait AutoSchemaDerivation[R] extends GenericSchema[R] with LowPriorityDerivedSchema {
  // for cross-compililing with scala 2
  inline def genAll[R, A]: Schema[R, A] = derived[R, A]
}

private[schema] trait LowPriorityDerivedSchema extends CommonSchemaDerivation {
  inline implicit def genAuto[R, A]: Schema[R, A] = derived[R, A]
}
