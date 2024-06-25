package caliban.schema.macros

import caliban.schema.Annotations.*
import caliban.schema.Schema

import scala.quoted.*

export magnolia1.TypeInfo

object Macros {
  inline def isFieldExcluded[P, T]: Boolean = ${ isFieldExcludedImpl[P, T] }
  inline def isEnumField[P, T]: Boolean     = ${ isEnumFieldImpl[P, T] }
  inline def implicitExists[T]: Boolean     = ${ implicitExistsImpl[T] }
  inline def hasAnnotation[T, Ann]: Boolean = ${ hasAnnotationImpl[T, Ann] }

  transparent inline def hasFieldsFromMethods[T]: Boolean =
    ${ hasFieldsFromMethodsImpl[T] }

  transparent inline def fieldsFromMethods[R, T]: List[(String, List[Any], Schema[R, ?])] =
    ${ fieldsFromMethodsImpl[R, T] }

  /**
   * Tests whether type argument [[FieldT]] in [[Parent]] is annotated with [[GQLExcluded]]
   */
  private def isFieldExcludedImpl[Parent: Type, FieldT: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val fieldName = Type.valueOfConstant[FieldT]
    val annSymbol = TypeRepr.of[GQLExcluded].typeSymbol
    Expr(TypeRepr.of[Parent].typeSymbol.primaryConstructor.paramSymss.flatten.exists { v =>
      fieldName.map(_ == v.name).getOrElse(false)
      && v.hasAnnotation(annSymbol)
    })
  }

  private def hasAnnotationImpl[T: Type, Ann: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val annSymbol = TypeRepr.of[Ann].typeSymbol
    Expr(TypeRepr.of[T].typeSymbol.hasAnnotation(annSymbol))
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

  private def hasFieldsFromMethodsImpl[T: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*
    val gqlFieldSym             = TypeRepr.of[GQLField].typeSymbol
    val gqlFieldsFromMethodsSym = TypeRepr.of[GQLFieldsFromMethods].typeSymbol

    Expr(
      TypeRepr.of[T].typeSymbol.hasAnnotation(gqlFieldsFromMethodsSym)
        || TypeTree.of[T].symbol.declaredMethods.exists(_.hasAnnotation(gqlFieldSym))
    )
  }

  private def fieldsFromMethodsImpl[R: Type, T: Type](using
    q: Quotes
  ): Expr[List[(String, List[Any], Schema[R, ?])]] = {
    import q.reflect.*
    val targetSym  = TypeTree.of[T].symbol
    val targetType = TypeRepr.of[T]

    val gqlFieldAnnType = TypeRepr.of[GQLField]
    val gqlFieldAnnSym  = gqlFieldAnnType.typeSymbol

    val gqlExcludedAnnSym          = TypeRepr.of[GQLExcluded].typeSymbol
    val gqlFieldsFromMethodsAnnSym = TypeRepr.of[GQLFieldsFromMethods].typeSymbol

    def summonSchema(methodSym: Symbol): Expr[Schema[R, ?]] = {
      val fieldType = targetType.memberType(methodSym)
      val tpe       = (fieldType match {
        case MethodType(_, _, returnType) => returnType
        case _                            => fieldType
      }).widen

      tpe.asType match {
        case '[f] =>
          Expr
            .summon[Schema[R, f]]
            .getOrElse(report.errorAndAbort(schemaNotFound(tpe.show)))
      }
    }

    def checkMethodNoArgs(methodSym: Symbol): Unit =
      if (methodSym.signature.paramSigs.size > 0)
        report.errorAndAbort(s"Method '${methodSym.name}' annotated with @GQLField must be parameterless")

    // Unfortunately we can't reuse Magnolias filtering so we copy the implementation
    def filterAnnotation(ann: Term): Boolean = {
      val tpe = ann.tpe
      val sym = tpe.typeSymbol

      tpe != gqlFieldAnnType && // No need to include the GQLField annotation
      (sym.maybeOwner.isNoSymbol ||
        (sym.owner.fullName != "scala.annotation.internal" &&
          sym.owner.fullName != "jdk.internal"))
    }

    def extractAnnotations(methodSym: Symbol): List[Expr[Any]] =
      methodSym.annotations.filter(filterAnnotation).map(_.asExpr.asInstanceOf[Expr[Any]])

    Expr.ofList {
      // NOTE: `Synthetic` methods are ones generated by the compiler, such as `copy`, `equals`, `hashCode`, etc.
      val allMethods = targetSym.declaredMethods.filterNot(_.flags.is(Flags.Synthetic))
      val filtered   =
        if targetType.typeSymbol.hasAnnotation(gqlFieldsFromMethodsAnnSym)
        then allMethods.filterNot(_.hasAnnotation(gqlExcludedAnnSym))
        else allMethods.filter(_.hasAnnotation(gqlFieldAnnSym))

      filtered.map { method =>
        checkMethodNoArgs(method)
        '{
          (
            ${ Expr(method.name) },
            ${ Expr.ofList(extractAnnotations(method)) },
            ${ summonSchema(method) }
          )
        }
      }
    }
  }

  // Copied from Schema so that we have the same compiler error message
  private inline def schemaNotFound(tpe: String) =
    s"""Cannot find a Schema for type $tpe.

Caliban provides instances of Schema for the most common Scala types, and can derive it for your case classes and sealed traits.
Derivation requires that you have a Schema for any other type nested inside $tpe.
If you use a custom type as an argument, you also need to provide an implicit ArgBuilder for that type.
See https://ghostdogpr.github.io/caliban/docs/schema.html for more information.
"""

}
