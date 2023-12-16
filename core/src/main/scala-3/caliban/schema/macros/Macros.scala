package caliban.schema.macros

import caliban.schema.Annotations.{ GQLExcluded, GQLField }
import caliban.schema.Schema

import scala.quoted.*

export magnolia1.TypeInfo

object Macros {
  inline def isFieldExcluded[P, T]: Boolean = ${ isFieldExcludedImpl[P, T] }
  inline def isEnumField[P, T]: Boolean     = ${ isEnumFieldImpl[P, T] }
  inline def implicitExists[T]: Boolean     = ${ implicitExistsImpl[T] }
  inline def hasAnnotation[T, Ann]: Boolean = ${ hasAnnotationImpl[T, Ann] }

  inline def fieldsFromMethods[R, T]: List[(String, Schema[R, ?])] = ${ fieldsFromMethodsImpl[R, T] }

  /**
   * Tests whether type argument [[FieldT]] in [[Parent]] is annotated with [[GQLExcluded]]
   */
  private def isFieldExcludedImpl[Parent: Type, FieldT: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val fieldName = Type.valueOfConstant[FieldT]
    val annSymbol = TypeRepr.of[GQLExcluded].typeSymbol
    Expr(TypeRepr.of[Parent].typeSymbol.primaryConstructor.paramSymss.flatten.exists { v =>
      fieldName.map(_ == v.name).getOrElse(false)
      && v.getAnnotation(annSymbol).isDefined
    })
  }

  private def hasAnnotationImpl[T: Type, Ann: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val annSymbol = TypeRepr.of[Ann].typeSymbol
    Expr(TypeRepr.of[T].typeSymbol.getAnnotation(annSymbol).isDefined)
  }

  private def implicitExistsImpl[T: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*
    Implicits.search(TypeRepr.of[T]) match {
      case _: ImplicitSearchSuccess => Expr(true)
      case _: ImplicitSearchFailure => Expr(false)
    }
  }

  private def isEnumFieldImpl[P: Type, T: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*
    Expr(TypeRepr.of[P].typeSymbol.flags.is(Flags.Enum) && TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))
  }

  private def fieldsFromMethodsImpl[R: Type, T: Type](using
    q: Quotes
  ): Expr[List[(String, Schema[R, ?])]] = {
    import q.reflect.*
    val targetSym  = TypeTree.of[T].symbol
    val targetType = TypeRepr.of[T]
    val annSymbol  = TypeRepr.of[GQLField].typeSymbol

    def summonSchema(methodSym: Symbol) = {
      val fieldType = targetType.memberType(methodSym)
      val tpe       = (fieldType match {
        case MethodType(_, _, returnType) => returnType
        case _                            => fieldType
      }).widen

      tpe.asType match {
        case '[f] =>
          Expr.summon[Schema[R, f]].getOrElse(report.errorAndAbort(s"Cannot find an instance of Schema for $tpe"))
      }
    }

    def checkMethodNoArgs(methodSym: Symbol): Unit =
      if (methodSym.signature.paramSigs.size > 0)
        report.errorAndAbort(s"Method '${methodSym.name}' annotated with @GQLField must be parameterless")

    Expr.ofList {
      targetSym.declaredMethods.filter(_.getAnnotation(annSymbol).isDefined).map { method =>
        checkMethodNoArgs(method)
        '{
          (
            ${ Expr(method.name) },
            ${ summonSchema(method) }
          )
        }
      }
    }
  }

}
