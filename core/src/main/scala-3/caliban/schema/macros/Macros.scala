package caliban.schema.macros

import scala.quoted.*

private[caliban] object Macros {
  inline def anns[T]: List[Any] = ${anns[T]}
  inline def paramAnns[T]: List[(String, List[Any])] = ${paramAnns[T]}

  def paramAnns[T: Type](using qctx: Quotes): Expr[List[(String, List[Any])]] = {
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

  def anns[T: Type](using qctx: Quotes): Expr[List[Any]] = {
    import qctx.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol || a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
      }.map(_.asExpr.asInstanceOf[Expr[Any]])
    }
  }
}
