package caliban.schema

import caliban.schema.Annotations.GQLExcluded
import caliban.schema.macros.TypeInfo

import scala.quoted.*
import scala.compiletime.*

private object Macros {
  // this code was inspired from WIP in magnolia
  // https://github.com/propensive/magnolia/blob/b937cf2c7dabebb8236e7e948f37a354777fa9b7/src/core/macro.scala

  inline def annotations[T]: List[Any]                      = ${ annotationsImpl[T] }
  inline def paramAnnotations[T]: List[(String, List[Any])] = ${ paramAnnotationsImpl[T] }
  inline def typeInfo[T]: TypeInfo                          = ${ typeInfoImpl[T] }
  inline def isFieldExcluded[P, T]: Boolean                 = ${ isFieldExcludedImpl[P, T] }
  inline def isEnumField[P, T]: Boolean                     = ${ isEnumFieldImpl[P, T] }
  inline def implicitExists[T]: Boolean                     = ${ implicitExistsImpl[T] }

  def annotationsImpl[T: Type](using qctx: Quotes): Expr[List[Any]] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol || (a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal" && a.tpe.typeSymbol.owner.fullName != "jdk.internal")
      }.map(_.asExpr.asInstanceOf[Expr[Any]])
    }
  }

  def paramAnnotationsImpl[T: Type](using qctx: Quotes): Expr[List[(String, List[Any])]] = {
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

  def typeInfoImpl[T: Type](using qctx: Quotes): Expr[TypeInfo] = {
    import qctx.reflect.*

    def normalizedName(s: Symbol): String = if (s.flags.is(Flags.Module)) s.name.stripSuffix("$") else s.name
    def name(tpe: TypeRepr): Expr[String] = Expr(normalizedName(tpe.typeSymbol))

    def ownerNameChain(sym: Symbol): List[String] =
      if (sym.isNoSymbol) List.empty
      else if (sym == defn.EmptyPackageClass) List.empty
      else if (sym == defn.RootPackage) List.empty
      else if (sym == defn.RootClass) List.empty
      else ownerNameChain(sym.owner) :+ normalizedName(sym)

    def owner(tpe: TypeRepr): Expr[String] = Expr(ownerNameChain(tpe.typeSymbol.maybeOwner).mkString("."))

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] = tpe match {
      case AppliedType(tpe, args) =>
        '{ TypeInfo(${ owner(tpe) }, ${ name(tpe) }, ${ Expr.ofList(args.map(typeInfo)) }) }
      case _                      =>
        '{ TypeInfo(${ owner(tpe) }, ${ name(tpe) }, Nil) }
    }

    typeInfo(TypeRepr.of[T])
  }

  /**
   * Tests whether type argument [[FieldT]] in [[Parent]] is annotated with [[GQLExcluded]]
   */
  def isFieldExcludedImpl[Parent: Type, FieldT: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeRepr.of[Parent].typeSymbol.primaryConstructor.paramSymss.flatten.exists { v =>
      Type.valueOfConstant[FieldT].map(_ == v.name).getOrElse(false)
      && v.annotations.exists(_.tpe =:= TypeRepr.of[GQLExcluded])
    })
  }

  def implicitExistsImpl[T: Type](using q: Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Implicits.search(TypeRepr.of[T]) match {
      case _: ImplicitSearchSuccess => Expr(true)
      case _: ImplicitSearchFailure => Expr(false)
    }
  }

  def isEnumFieldImpl[P: Type, T: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*
    Expr(TypeRepr.of[P].typeSymbol.flags.is(Flags.Enum) && TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))
  }

}
