package caliban.schema

object DerivedMagnolia {
  import magnolia.Magnolia

  import scala.reflect.macros.whitebox

  // Wrap the output of Magnolia in a Derived to force it to a lower priority.
  // This seems to work, despite magnolia hardcode checks for `macroApplication` symbol
  // and relying on getting an diverging implicit expansion error for auto-mode.
  // Thankfully at least it doesn't check the output type of its `macroApplication`
  def derivedMagnolia[TC[_], A: c.WeakTypeTag](c: whitebox.Context): c.Expr[Derived[TC[A]]] = {
    val magnoliaTree = c.Expr[TC[A]](Magnolia.gen[A](c))
    c.universe.reify(Derived(magnoliaTree.splice))
  }
}
