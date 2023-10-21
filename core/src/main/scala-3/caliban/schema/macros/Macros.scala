package caliban.schema.macros

import caliban.schema.Annotations.GQLExcluded

import scala.quoted.*

export magnolia1.TypeInfo

object Macros {
  inline def isFieldExcluded[P, T]: Boolean = ${ isFieldExcludedImpl[P, T] }
  inline def isEnumField[P, T]: Boolean     = ${ isEnumFieldImpl[P, T] }
  inline def implicitExists[T]: Boolean     = ${ implicitExistsImpl[T] }

  /**
   * Tests whether type argument [[FieldT]] in [[Parent]] is annotated with [[GQLExcluded]]
   */
  private def isFieldExcludedImpl[Parent: Type, FieldT: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val fieldName = Type.valueOfConstant[FieldT]
    Expr(TypeRepr.of[Parent].typeSymbol.primaryConstructor.paramSymss.flatten.exists { v =>
      fieldName.map(_ == v.name).getOrElse(false)
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
