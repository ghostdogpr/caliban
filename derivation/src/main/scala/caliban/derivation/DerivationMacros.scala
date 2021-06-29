package caliban.derivation

import caliban.derivation.annotation.GQLExclude

import scala.reflect.macros.blackbox

/**
 * Macro bundle for deriving instances of Schema for concrete classes,
 * representing public members as either regular fields or functions, depending on whether or not
 * the member takes parameters.
 */
class DerivationMacros(val c: blackbox.Context) extends CalibanUtils {
  import c.universe._

  private case class DeriveMember(sym: TermSymbol) {
    // n.b. To remain consistent with the magnolia derivation, we ignore the GQLName and GQLInputName annotations
    // on fields, methods, and parameters. Though I don't see a reason why we cannot use them for these cases as well.

    // Only accept methods that have a single (non-implicit) parameter list
    assert(!sym.isMethod || sym.asMethod.paramLists.count(nonImplicit) <= 1)
    // Only accept methods that do not have type parameters
    assert(!sym.isMethod || sym.asMethod.typeParams.isEmpty)

    private val params: Option[List[Symbol]] =
      if (sym.isMethod) sym.asMethod.paramLists.headOption
      else None

    private val scalaType: Type = if (sym.isMethod) sym.asMethod.returnType else sym.typeSignature

    private val info: GraphQLInfo = GraphQLInfo(sym)

    private val fieldName: TermName = sym.name.normalize

    private val fieldNameStr: Tree = mkConst(sym.name.normalizeString)

    // TODO: Figure out how to cleanly extract the default argument, if any
    def deriveParam: Tree = Schema.summon(scalaType) { paramSchema =>
      q"""
        ${refs.__InputValue}(
          $fieldNameStr,
          ${info.description},
          ${mkCalibanType(Ident(paramSchema), isInput = true)},
          None,
          ${info.directivesOpt}
        )
      """
    }

    def deriveField: Tree = Schema.summon(scalaType) { schema =>
      val args =
        params.filterNot(_.isEmpty) match {
          case Some(ps) =>
            // Non-empty list of parameters
            val argTrees = ps.map(p => DeriveMember(p.asTerm).deriveParam)
            q"List(..$argTrees)"

          case None =>
            // Parameterless or empty parameter list
            q"Nil"
        }

      val tpe = mkCalibanType(Ident(schema), isInput = false)

      q"""
        ${refs.__Field}(
          $fieldNameStr,
          ${info.description},
          $args,
          $tpe,
          ${info.isDeprecated},
          ${info.deprecationReason},
          ${info.directivesOpt}
        )
      """
    }

    def deriveStep(parent: Type, resolveValue: TermName): Tree = Schema.summon(scalaType) { schema =>
      val emptyParamList: Boolean =
        params.exists(_.isEmpty)

      params.filterNot(_.isEmpty) match {
        case Some(ps) =>
          // Non-empty list of parameters

          // Generate a function from the InputValue's received to a Step for resolving the method call
          val functionStep = mkFunction() { args =>
            val paramNames = ps.map(p => c.freshName(p.name))

            val builtArgs =
              ps.zip(paramNames).map {
                case (p, name) =>
                  val paramName = p.nameStringLit
                  val rhs       = ArgBuilder.summon(p.typeSignature)(bld => q"$bld.build($args($paramName))")

                  fq"$name <- $rhs"
              }

            val forExpr = q"for (..$builtArgs) yield $resolveValue.$fieldName(..$paramNames)"

            q"""
              $forExpr match {
                case Left(error) => ${refs.QueryStep}(${refs.ZQuery}.fail(error))
                case Right(value) => $schema.resolve(value)
              }
            """
          }

          q"${refs.FunctionStep}($functionStep)"

        case None =>
          // Parameterless or empty parameter list
          val invoke =
            if (emptyParamList) q"$resolveValue.$fieldName()"
            else q"$resolveValue.$fieldName"

          q"$schema.resolve($invoke)"
      }
    }

    def deriveStepWithName(parent: Type, resolveValue: TermName): Tree =
      q"($fieldNameStr, ${deriveStep(parent, resolveValue)})"
  }

  private def nonImplicit(params: List[Symbol]): Boolean =
    !params.exists(_.isImplicit)

  private def canDeriveOutput(sym: TermSymbol): Boolean =
    (!sym.isMethod || sym.asMethod.typeParams.isEmpty) &&
      sym.isPublic &&
      !sym.isConstructor &&
      !sym.isSynthetic

  private def canDeriveInput(sym: TermSymbol): Boolean =
    sym.isPublic && sym.isCaseAccessor

  /**
   * Retrieve the underlying symbol for the accessor that holds the annotations
   *
   * For a case class parameter, this will be the corresponding constructor parameter,
   * and for regular vals/vars it will be the underlying val/var symbol.
   */
  private def underlyingField(tpe: Type, ms: MethodSymbol): TermSymbol =
    if (ms.isCaseAccessor) {
      val cons = tpe.decls.collectFirst {
        case ms: MethodSymbol if ms.isPrimaryConstructor => ms
      }.get

      cons.paramLists.head.find(_.name == ms.name).get.asTerm
    } else if (!ms.isGetter) ms
    else {
      tpe.decls.collectFirst {
        case sym: TermSymbol if !sym.isMethod && sym.name.normalizeString == ms.nameString => sym
      }.get
    }

  private def inputFields(tpe: Type): List[TermSymbol] =
    tpe.decls.collect { case ms: MethodSymbol => ms }
      .filter(canDeriveInput)
      .map(underlyingField(tpe, _))
      .toList

  private def outputFields(tpe: Type): List[TermSymbol] =
    tpe.decls.collect { case ms: MethodSymbol => ms }
      .filter(canDeriveOutput)
      .map(underlyingField(tpe, _))
      .filterNot(hasAnnotation[GQLExclude])
      .toList

  private def deriveInput(info: GraphQLInfo, inputs: List[TermSymbol]): Tree = {
    val fields =
      inputs.map(i => DeriveMember(i).deriveParam)

    q"""
      ${refs.makeInputObject}(
        Some(${info.inputName}),
        ${info.description},
        List(..$fields)
      )
    """
  }

  private def deriveOutput(info: GraphQLInfo, outputs: List[TermSymbol]): Tree = {
    val fields =
      outputs.map(o => DeriveMember(o).deriveField)

    q"""
      ${refs.makeObject}(
        Some(${info.name}),
        ${info.description},
        List(..$fields),
        ${info.directives}
      )
    """
  }

  private def deriveStep(parent: Type, info: GraphQLInfo, resolveValue: TermName, outputs: List[TermSymbol]): Tree = {
    val fields =
      outputs.map(o => DeriveMember(o).deriveStepWithName(parent, resolveValue))

    q"""
      ${refs.ObjectStep}(
        ${info.name},
        Map(..$fields)
      )
    """
  }

  def deriveSchema[T](implicit wtt: WeakTypeTag[T]): Tree = {
    val tpe = wtt.tpe

    if (!tpe.typeSymbol.isClass || tpe.typeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, s"Only concrete classes can currently be derived with the `deriveSchema` macro")
    }

    val inputs  = inputFields(tpe)
    val outputs = outputFields(tpe)

    val resolveValue = TermName(c.freshName())

    val info = GraphQLInfo(tpe.typeSymbol)
    val toType =
      q"""
        override def toType(isInput: Boolean = false, isSubscription: Boolean = false): ${typeRefs.__Type} =
          if (isInput) ${deriveInput(info, inputs)}
          else ${deriveOutput(info, outputs)}
      """
    val resolve =
      q"""
         override def resolve($resolveValue: $tpe): ${Step(typeRefs.Any)} =
           ${deriveStep(tpe, info, resolveValue, outputs)}
      """

    q"""
      new ${Schema(tpe)} {
        $toType
        $resolve
      }
    """
  }
}
