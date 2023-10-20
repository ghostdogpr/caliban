package caliban.schema.macros

import caliban.schema.Annotations.GQLExcluded

import scala.quoted.*

export magnolia1.TypeInfo

object Macros {
  inline def annotations[T]: List[Any]                      = ${ annotationsImpl[T] }
  inline def paramAnnotations[T]: List[(String, List[Any])] = ${ paramAnnotationsImpl[T] }
  inline def isFieldExcluded[P, T]: Boolean                 = ${ isFieldExcludedImpl[P, T] }
  inline def isEnumField[P, T]: Boolean                     = ${ isEnumFieldImpl[P, T] }
  inline def implicitExists[T]: Boolean                     = ${ implicitExistsImpl[T] }

  private def annotationsImpl[T: Type](using qctx: Quotes): Expr[List[Any]] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol || (a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal" && a.tpe.typeSymbol.owner.fullName != "jdk.internal")
      }.map(_.asExpr.asInstanceOf[Expr[Any]])
    }
  }

  private def paramAnnotationsImpl[T: Type](using qctx: Quotes): Expr[List[(String, List[Any])]] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr.ofList {
      tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            (a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal" && a.tpe.typeSymbol.owner.fullName != "jdk.internal")
        }.map(_.asExpr.asInstanceOf[Expr[Any]])
      }.filter(_._2.nonEmpty).map((name, anns) => Expr.ofTuple(name, Expr.ofList(anns)))
    }
  }

  /**
   * Tests whether type argument [[FieldT]] in [[Parent]] is annotated with [[GQLExcluded]]
   */
  private def isFieldExcludedImpl[Parent: Type, FieldT: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeRepr.of[Parent].typeSymbol.primaryConstructor.paramSymss.flatten.exists { v =>
      Type.valueOfConstant[FieldT].map(_ == v.name).getOrElse(false)
      && v.annotations.exists(_.tpe =:= TypeRepr.of[GQLExcluded])
    })
  }

  private def implicitExistsImpl[T: Type](using q: Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Implicits.search(TypeRepr.of[T]) match {
      case _: ImplicitSearchSuccess => Expr(true)
      case _: ImplicitSearchFailure => Expr(false)
    }
  }

  private def isEnumFieldImpl[P: Type, T: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*
    Expr(TypeRepr.of[P].typeSymbol.flags.is(Flags.Enum) && TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))
  }

}
