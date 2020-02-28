package caliban.derivation

import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLDirective, GQLInputName, GQLName }

/**
 * Caliban-specific macro utilities
 */
trait CalibanUtils extends MacroUtils {

  import c.universe._

  protected case class GraphQLInfo(
    name: Tree,
    inputName: Tree,
    description: Tree,
    deprecationReason: Tree,
    directives: Tree
  ) {
    def isDeprecated: Tree = q"$deprecationReason.isDefined"

    def directivesOpt: Tree = q"Some($directives).filterNot(_.isEmpty)"
  }

  protected object GraphQLInfo {
    private val GQLNameType        = OneArgumentAnnotation[GQLName]
    private val GQLDescriptionType = OneArgumentAnnotation[GQLDescription]
    private val GQLDeprecatedType  = OneArgumentAnnotation[GQLDeprecated]
    private val GQLDirectiveType   = OneArgumentAnnotation[GQLDirective]
    private val GQLInputNameType   = OneArgumentAnnotation[GQLInputName]

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

    private def getDirectives(s: Symbol): Tree = {
      val dirs = s.scalaAnnotations.collect {
        case GQLDirectiveType(dir) => dir
      }

      q"List(..$dirs)"
    }

    private def getInputName(s: Symbol): Tree =
      s.scalaAnnotations.collectFirst {
        case GQLInputNameType(name) => name
      }.getOrElse(q"${refs.customizeInputTypeName}(${getName(s)})")

    def apply(s: Symbol): GraphQLInfo = {
      assert(s != NoSymbol)

      GraphQLInfo(getName(s), getInputName(s), getDescription(s), getDeprecation(s), getDirectives(s))
    }
  }

  protected object refs {
    val __Field: RefTree                = companionRef[caliban.introspection.adt.__Field]
    val __InputValue: RefTree           = companionRef[caliban.introspection.adt.__InputValue]
    val customizeInputTypeName: RefTree = companionRef[caliban.schema.Schema[_, _]].select("customizeInputTypeName")
    val makeNonNull: RefTree            = companionRef[caliban.schema.Types.type].select("makeNonNull")
    val makeObject: RefTree             = companionRef[caliban.schema.Types.type].select("makeObject")
    val makeInputObject: RefTree        = companionRef[caliban.schema.Types.type].select("makeInputObject")
    val FunctionStep: RefTree           = companionRef[caliban.schema.Step.FunctionStep[_]]
    val ObjectStep: RefTree             = companionRef[caliban.schema.Step.ObjectStep[_]]
    val QueryStep: RefTree              = companionRef[caliban.schema.Step.QueryStep[_]]
    val ZQuery: RefTree                 = companionRef[zquery.ZQuery[_, _, _]]
  }

  protected object typeRefs {
    val __Type: Type = typeOf[caliban.introspection.adt.__Type]
    val Any: Type    = typeOf[Any]
  }

  protected object Schema extends HKType {
    private val sym: Symbol = symbolOf[caliban.schema.Schema[_, _]]

    def apply(tpe: Type): Type = internal.typeRef(NoPrefix, sym, List(typeRefs.Any, tpe))

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

  protected object Step extends HKType {
    private val sym: Symbol = symbolOf[caliban.schema.Step[_]]

    def apply(tpe: Type): Type = internal.typeRef(NoPrefix, sym, List(tpe))

    def unapply(tpe: Type): Option[Type] =
      tpe match {
        case TypeRef(_, `sym`, List(x)) => Some(x)
        case _                          => None
      }
  }

  protected def mkCalibanType(schema: Tree, isInput: Boolean): Tree = {
    val isInputTree = mkConst(isInput)

    q"""
      () =>
        if ($schema.optional)
          $schema.toType($isInputTree)
        else
          ${refs.makeNonNull}($schema.toType($isInputTree))
    """
  }
}
