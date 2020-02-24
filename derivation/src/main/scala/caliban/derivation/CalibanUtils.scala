package caliban.derivation

import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }

/**
 * Caliban-specific macro utilities
 */
trait CalibanUtils extends MacroUtils {
  import c.universe._

  protected case class GraphQLInfo(
    name: Tree,
    description: Tree,
    deprecationReason: Tree
  ) {
    def isDeprecated: Tree = q"$deprecationReason.isDefined"
  }

  protected object GraphQLInfo {
    private val GQLNameType        = OneArgumentAnnotation[GQLName]
    private val GQLDescriptionType = OneArgumentAnnotation[GQLDescription]
    private val GQLDeprecatedType  = OneArgumentAnnotation[GQLDeprecated]

    private def getName(s: Symbol): Tree = {
      val default =
        if (s.isType) {
          mkConst(s.nameString + s.asType.typeParams.map(_.nameString).mkString)
        } else s.nameStringLit

      s.scalaAnnotations.collectFirst {
        case GQLNameType(name) => name
      }.getOrElse(default)
    }

    private def getDescription(s: Symbol): Tree =
      s.scalaAnnotations.collectFirst {
        case GQLDescriptionType(desc) => q"Some($desc)"
      }.getOrElse(q"None")

    private def getDeprecation(s: Symbol): Tree =
      s.scalaAnnotations.collectFirst {
        case GQLDeprecatedType(reason) => q"Some($reason)"
      }.getOrElse(q"None")

    def apply(s: Symbol): GraphQLInfo = {
      assert(s != NoSymbol)

      GraphQLInfo(getName(s), getDescription(s), getDeprecation(s))
    }
  }

  protected object Ref {
    val __Field: RefTree      = companionRef[caliban.introspection.adt.__Field]
    val __InputValue: RefTree = companionRef[caliban.introspection.adt.__InputValue]
    val objectSchema: RefTree = companionRef[caliban.schema.Schema[_, _]].select("objectSchema")
    val makeNonNull: RefTree  = companionRef[caliban.schema.Types.type].select("makeNonNull")
    val FunctionStep: RefTree = companionRef[caliban.schema.Step.FunctionStep[_]]
    val QueryStep: RefTree    = companionRef[caliban.schema.Step.QueryStep[_]]
    val ZQuery: RefTree       = companionRef[zquery.ZQuery[_, _, _]]
  }

  protected object Schema extends HKType {
    private val sym: Symbol = symbolOf[caliban.schema.Schema[_, _]]
    private val any: Type   = typeOf[Any]

    def apply(tpe: Type): Type = internal.typeRef(NoPrefix, sym, List(any, tpe))
    def unapply(tpe: Type): Option[Type] =
      tpe match {
        case TypeRef(_, `sym`, List(_, x)) => Some(x)
        case _                             => None
      }
  }

  protected object ArgBuilder extends HKType {
    private val sym: Symbol = symbolOf[caliban.schema.ArgBuilder[_]]

    def apply(tpe: Type): Type = internal.typeRef(NoPrefix, sym, List(tpe))
    def unapply(tpe: Type): Option[Type] =
      tpe match {
        case TypeRef(_, `sym`, List(x)) => Some(x)
        case _                          => None
      }
  }

  protected def mkCalibanType(schema: Tree, isInput: Boolean = false): Tree = {
    val isInputTree = mkConst(isInput)

    q"""
      () =>
        if ($schema.optional)
          $schema.toType($isInputTree)
        else
          ${Ref.makeNonNull}($schema.toType($isInputTree))
    """
  }
}
