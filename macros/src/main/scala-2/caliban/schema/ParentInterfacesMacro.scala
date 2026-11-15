package caliban.schema

import scala.reflect.macros.whitebox

object ParentInterfacesMacro {
  def materialize[R: c.WeakTypeTag, T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    def isInterface(s: Symbol): Boolean =
      s.isClass && s.asClass.isSealed && {
        s.info
        s.annotations.exists(_.tree.tpe.typeSymbol.fullName == "caliban.schema.Annotations.GQLInterface")
      }

    val r       = weakTypeOf[R]
    val tpe     = weakTypeOf[T]
    val sym     = tpe.typeSymbol
    val schema  = c.mirror.staticClass("caliban.schema.Schema").toTypeConstructor
    val parents =
      if (!isInterface(sym)) Nil
      else
        sym.asClass.baseClasses.tail
          .filter(isInterface)
          .map(p => c.inferImplicitValue(appliedType(schema, r, tpe.baseType(p)), silent = true))
          .filter(t => t.nonEmpty && !t.exists(s => s.tpe != null && s.tpe.typeSymbol == symbolOf[Derived[_]]))

    q"new _root_.caliban.schema.ParentInterfaces[$r, $tpe](() => _root_.scala.List(..$parents))"
  }
}
