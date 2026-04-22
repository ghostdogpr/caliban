package caliban.schema.internal.macros

import hearth.MacroCommonsScala2

trait ArgBuilderAnnotationSupportScala2 extends ArgBuilderAnnotationSupport { this: MacroCommonsScala2 =>
  override protected def hasTypeAnnotation[A: Type, Ann: Type]: Boolean = {
    val annTpe = UntypedType.fromTyped[Ann]
    UntypedType
      .fromTyped[A]
      .annotations
      .exists(_.tpe =:= annTpe)
  }
}
