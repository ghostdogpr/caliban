package caliban.schema.macros

import scala.quoted.*

private[caliban] object Macros {
  // this code was inspired from WIP in magnolia
  // https://github.com/propensive/magnolia/blob/b937cf2c7dabebb8236e7e948f37a354777fa9b7/src/core/macro.scala

  inline def annotations[T]: List[Any] = ${annotationsImpl[T]}
  inline def paramAnnotations[T]: List[(String, List[Any])] = ${paramAnnotationsImpl[T]}
  inline def isValueClass[T]: Boolean = ${isValueClassImpl[T]}
  inline def typeInfo[T]: TypeInfo = ${typeInfoImpl[T]}
  inline def isObject[T]: Boolean = ${isObjectImpl[T]}

  def annotationsImpl[T: Type](using qctx: Quotes): Expr[List[Any]] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol || a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
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
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        }.map(_.asExpr.asInstanceOf[Expr[Any]])
      }.filter(_._2.nonEmpty).map { (name, anns) => Expr.ofTuple(name, Expr.ofList(anns)) }
    }
  }

  def isValueClassImpl[T: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeRepr.of[T].baseClasses.contains(Symbol.classSymbol("scala.AnyVal")))
  }

    def typeInfoImpl[T: Type](using qctx: Quotes): Expr[TypeInfo] = {
      import qctx.reflect._

      def normalizedName(s: Symbol): String = if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
      def name(tpe: TypeRepr) : Expr[String] = Expr(normalizedName(tpe.typeSymbol))

      def owner(tpe: TypeRepr): Expr[String] =
        if tpe.typeSymbol.maybeOwner.isNoSymbol then Expr("<no owner>")
        else if (tpe.typeSymbol.owner == defn.EmptyPackageClass) Expr("")
        else Expr(tpe.typeSymbol.owner.name)

      def typeInfo(tpe: TypeRepr): Expr[TypeInfo] = tpe match
        case AppliedType(tpe, args) =>
          '{TypeInfo(${owner(tpe)}, ${name(tpe)}, ${Expr.ofList(args.map(typeInfo))})}
        case _ =>
          '{TypeInfo(${owner(tpe)}, ${name(tpe)}, Nil)}

      typeInfo(TypeRepr.of[T])
    }

  def isObjectImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
  }
}
