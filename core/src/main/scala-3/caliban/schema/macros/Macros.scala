package caliban.schema.macros

import scala.quoted.*

private[caliban] object Macros {
  inline def annotations[T]: List[Any] = ${annotationsImpl[T]}
  inline def paramAnnotations[T]: List[(String, List[Any])] = ${paramAnnotationsImpl[T]}

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
}
