package caliban.derivation

import scala.reflect.macros.blackbox

/**
 * Generic macro utilities
 */
trait MacroUtils {
  val c: blackbox.Context

  import c.universe._

  protected trait HKType {
    def apply(tpe: Type): Type
    def unapply(tpe: Type): Option[Type]
    def summon(tpe: Type): (TermName => Tree) => Tree = withTypeclass(this)(tpe)
  }

  protected trait HKType2 {
    def apply(tpe1: Type, tpe2: Type): Type
    def unapply(tpe: Type): Option[(Type, Type)]
    def summon(tpe1: Type, tpe2: Type): (TermName => Tree) => Tree = withTypeclass2(this)(tpe1, tpe2)
  }

  protected class OneArgumentAnnotation(tpe: Type) {
    def unapply(annotation: Annotation): Option[Tree] =
      annotation.tree match {
        case Apply(_, List(arg)) if annotation.tree.tpe =:= tpe => Some(arg)
        case _                                                  => None
      }
  }

  protected object OneArgumentAnnotation {
    def apply[T: TypeTag] = new OneArgumentAnnotation(typeOf[T])
  }

  protected implicit class RefTreeOps(rt: RefTree) {
    def select(name: TermName): RefTree = Select(rt, name)
    def select(name: String): RefTree   = select(TermName(name))
  }

  protected implicit class SymbolOps(s: Symbol) {
    private def isJavaAnnotation(annotation: Annotation): Boolean =
      annotation.tree.tpe <:< typeOf[java.lang.annotation.Annotation]

    def nameString: String                 =
      if (s.isTerm) s.name.toTermName.normalizeString
      else s.name.decodedName.toString
    def nameStringLit: Tree                = mkConst(nameString)
    def scalaAnnotations: List[Annotation] = s.annotations.filterNot(isJavaAnnotation)
  }

  protected implicit class TermNameOps(name: TermName) {

    /**
     * If the name is that of a local val/var, remove the (internal) suffix indicating this,
     * giving the name of the associated getter.
     */
    def normalize: TermName =
      TermName(normalizeString)

    def normalizeString: String =
      decodedString.stripSuffix(termNames.LOCAL_SUFFIX_STRING)

    def decodedString: String =
      name.decodedName.toString
  }

  protected def mkParam(name: TermName, tpe: Type = null): ValDef =
    ValDef(Modifiers(Flag.PARAM), name, TypeTree(tpe), EmptyTree)

  protected def mkConst(value: Any): Tree =
    Literal(Constant(value))

  private def absolutePath(sym: Symbol): RefTree =
    sym.fullName.split("\\.").foldLeft(Ident(termNames.ROOTPKG): RefTree) { case (prefix, elem) =>
      Select(prefix, TermName(elem))
    }

  protected def companionRef[T: WeakTypeTag]: RefTree = {
    val sym = symbolOf[T]

    val companionSym = if (sym.isModuleClass) sym else sym.companion
    absolutePath(companionSym)
  }

  protected def withTypeclass(hktype: HKType)(tpe: Type)(f: TermName => Tree): Tree = {
    val name: TermName    = TermName(c.freshName())
    val appliedType: Type = hktype(tpe)

    q"""
      val $name: $appliedType = implicitly[$appliedType]

      ${f(name)}
    """
  }

  protected def withTypeclass2(hktype: HKType2)(env: Type, tpe: Type)(f: TermName => Tree): Tree = {
    val name: TermName    = TermName(c.freshName())
    val appliedType: Type = hktype(env, tpe)

    q"""
      val $name: $appliedType = implicitly[$appliedType]

      ${f(name)}
    """
  }

  protected def hasAnnotation[T: TypeTag](s: Symbol): Boolean =
    s.scalaAnnotations.exists(_.tree.tpe <:< typeOf[T])

  protected def mkFunction(tpe: Type = null)(f: TermName => Tree): Tree = {
    val name: TermName = TermName(c.freshName())
    val param: ValDef  = mkParam(name, tpe)

    q"{ $param => ${f(name)} }"
  }

  protected def knownSubclassesOf(parent: ClassSymbol): Set[Symbol] = {
    // Based on Magnolia's gen function
    val (abstractChildren, concreteChildren) = parent.knownDirectSubclasses.partition(_.isAbstract)
    for (child <- concreteChildren) {
      child.typeSignature // load type signature
      if (!child.isFinal && !child.asClass.isCaseClass)
        c.abort(c.enclosingPosition, s"child $child of $parent is neither final nor a case class")
    }

    concreteChildren union abstractChildren.flatMap { child =>
      child.typeSignature // load type signature
      val childClass = child.asClass
      if (childClass.isSealed) knownSubclassesOf(childClass)
      else c.abort(c.enclosingPosition, s"child $child of $parent is not sealed")
    }
  }
}
