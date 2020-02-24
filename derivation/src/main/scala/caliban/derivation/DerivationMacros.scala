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

  private case class DeriveMember(parent: Type, ms: MethodSymbol) {
    assert(ms.paramLists.count(nonImplicit) <= 1)
    assert(ms.typeParams.isEmpty)

    private val params: Option[List[Symbol]] = ms.paramLists.headOption

    private val scalaType: Type = ms.returnType

    private val info: GraphQLInfo = GraphQLInfo(ms)

    private val fieldName: TermName = ms.name.toTermName

    private def deriveParam(param: Symbol): Tree = {
      // TODO: Figure out how to cleanly extract the default argument, if any

      val info = GraphQLInfo(param)

      Schema.summon(param.typeSignature) { paramSchema =>
        q"""
          ${Ref.__InputValue}(
            ${info.name},
            ${info.description},
            ${mkCalibanType(Ident(paramSchema))},
            None
          )
        """
      }
    }

    private def deriveField(schema: TermName): Tree = {
      val args =
        params.filterNot(_.isEmpty) match {
          case Some(ps) =>
            // Non-empty list of parameters
            val argTrees = ps.map(deriveParam)
            q"List(..$argTrees)"

          case None =>
            // Parameterless or empty parameter list
            q"Nil"
        }

      val tpe = mkCalibanType(Ident(schema))

      q"""
        ${Ref.__Field}(
          ${info.name},
          ${info.description},
          $args,
          $tpe,
          ${info.isDeprecated},
          ${info.deprecationReason}
        )
      """
    }

    private def deriveStep(schema: TermName): Tree = {
      val emptyParamList: Boolean =
        params.exists(_.isEmpty)

      // Generate a function from the enclosing case class instance to a Step for resolving this field
      mkFunction(parent) { parentVar =>
        params.filterNot(_.isEmpty) match {
          case Some(ps) =>
            // Non-empty list of parameters

            // Generate a function from the InputValue's received to a Step for resolving the method call
            val functionStep = mkFunction() { args =>
              val paramNames = ps.map(p => c.freshName(p.name))

              val builtArgs =
                ps.zip(paramNames).map {
                  case (p, name) =>
                    val paramInfo = GraphQLInfo(p)

                    val rhs = ArgBuilder.summon(p.typeSignature)(bld => q"$bld.build($args(${paramInfo.name}))")

                    fq"$name <- $rhs"
                }

              val forExpr = q"for (..$builtArgs) yield $parentVar.$fieldName(..$paramNames)"

              q"""
                $forExpr match {
                  case Left(error) => ${Ref.QueryStep}(${Ref.ZQuery}.fail(error))
                  case Right(value) => $schema.resolve(value)
                }
              """
            }

            q"${Ref.FunctionStep}($functionStep)"

          case None =>
            // Parameterless or empty parameter list
            val invoke =
              if (emptyParamList) q"$parentVar.$fieldName()"
              else q"$parentVar.$fieldName"

            q"$schema.resolve($invoke)"
        }
      }
    }

    def derive: Tree =
      Schema.summon(scalaType)(schema => q"(${deriveField(schema)}, ${deriveStep(schema)})")
  }

  private def nonImplicit(params: List[Symbol]): Boolean =
    !params.exists(_.isImplicit)

  private def canDerive(method: MethodSymbol): Boolean =
    method.typeParams.isEmpty &&
      method.isPublic &&
      !method.isConstructor &&
      !method.isSynthetic &&
      !hasAnnotation[GQLExclude](method)

  def deriveSchema[T](implicit wtt: WeakTypeTag[T]): Tree = {
    val tpe = wtt.tpe

    if (!tpe.typeSymbol.isClass || tpe.typeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, s"Only concrete classes can currently be derived with the `deriveSchema` macro")
    }

    val candidates = tpe.decls.collect {
      case ms: MethodSymbol if canDerive(ms) => ms
    }

    val info   = GraphQLInfo(tpe.typeSymbol)
    val fields = candidates.map(DeriveMember(tpe, _).derive)

    q"""
      ${Ref.objectSchema}(${info.name}, ${info.description}, List(..$fields))
    """
  }
}
