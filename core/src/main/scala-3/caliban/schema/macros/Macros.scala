package caliban.schema.macros

import caliban.schema.Annotations.{ GQLInterface, GQLUnion, GQLValueType }

import scala.quoted.*

private[caliban] object Macros {
  // this code was inspired from WIP in magnolia
  // https://github.com/propensive/magnolia/blob/b937cf2c7dabebb8236e7e948f37a354777fa9b7/src/core/macro.scala

  inline def annotations[T]: List[Any]                      = ${ annotationsImpl[T] }
  inline def paramAnnotations[T]: List[(String, List[Any])] = ${ paramAnnotationsImpl[T] }
  inline def typeInfo[T]: TypeInfo                          = ${ typeInfoImpl[T] }
  inline def isValueType[T]: Boolean                        = ${ isValueTypeImpl[T] }
  inline def isScalarValueType[T]: Boolean                  = ${ isScalarValueTypeImpl[T] }
  inline def isUnion[T]: Boolean                            = ${ isUnionImpl[T] }
  inline def isInterface[T]: Boolean                        = ${ isInterfaceImpl[T] }
  inline def hasFields[Label]: Boolean                      = ${ hasFieldsImpl[Label] }

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

    def normalizedName(s: Symbol): String = if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
    def name(tpe: TypeRepr): Expr[String] = Expr(normalizedName(tpe.typeSymbol))

    def ownerNameChain(sym: Symbol): List[String] =
      if sym.isNoSymbol then List.empty
      else if sym == defn.EmptyPackageClass then List.empty
      else if sym == defn.RootPackage then List.empty
      else if sym == defn.RootClass then List.empty
      else ownerNameChain(sym.owner) :+ normalizedName(sym)

    def owner(tpe: TypeRepr): Expr[String] = Expr(ownerNameChain(tpe.typeSymbol.maybeOwner).mkString("."))

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] = tpe match
      case AppliedType(tpe, args) =>
        '{ TypeInfo(${ owner(tpe) }, ${ name(tpe) }, ${ Expr.ofList(args.map(typeInfo)) }) }
      case _                      =>
        '{ TypeInfo(${ owner(tpe) }, ${ name(tpe) }, Nil) }

    typeInfo(TypeRepr.of[T])
  }

  def isValueTypeImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr(tpe.typeSymbol.annotations.exists { v =>
      v.tpe =:= TypeRepr.of[GQLValueType]
    })
  }

  def isScalarValueTypeImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr(tpe.typeSymbol.annotations.exists { v =>
      v.asExpr match
        case '{ new GQLValueType($isScalar) } =>
          isScalar.value match
            case Some(v: Boolean) => v
            case None => GQLValueType().isScalar // Default value
        case _                                => false
    })
  }

  def isUnionImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr(tpe.typeSymbol.annotations.exists { v =>
      v.tpe =:= TypeRepr.of[GQLUnion]
    })
  }

  def isInterfaceImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr(tpe.typeSymbol.annotations.exists { v =>
      v.tpe =:= TypeRepr.of[GQLInterface]
    })
  }

  def hasFieldsImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(!(TypeRepr.of[T] =:= TypeRepr.of[EmptyTuple]))
  }

}
