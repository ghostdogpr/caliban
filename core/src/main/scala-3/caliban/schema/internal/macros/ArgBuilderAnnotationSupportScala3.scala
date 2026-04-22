package caliban.schema.internal.macros

import hearth.MacroCommonsScala3

trait ArgBuilderAnnotationSupportScala3 extends ArgBuilderAnnotationSupport { this: MacroCommonsScala3 =>
  import quotes.reflect._

  override protected def hasTypeAnnotation[A: Type, Ann: Type]: Boolean = {
    val annTpe = UntypedType.fromTyped[Ann]
    TypeRepr.of[A].typeSymbol.annotations.exists(_.tpe =:= annTpe)
  }
}
