package caliban.schema.internal.macros

import hearth.MacroCommons

trait ArgBuilderAnnotationSupport { this: MacroCommons =>

  protected def hasTypeAnnotation[A: Type, Ann: Type]: Boolean

  protected final def annotationsExpr(annotations: List[Expr_??]): Expr[List[Any]] =
    annotations.foldRight(Expr.quote(List.empty[Any])) { (annotation, acc) =>
      import annotation.{ Underlying => Annotation, value => annotationExpr }
      Expr.quote(Expr.splice(annotationExpr).asInstanceOf[Any] :: Expr.splice(acc))
    }

  protected final def typeAnnotationsExpr[A: Type]: Expr[List[Any]] =
    annotationsExpr(Type.annotations[A])

  protected final def paramAnnotationsExpr(param: Parameter): Expr[List[Any]] =
    annotationsExpr(param.annotations)
}
