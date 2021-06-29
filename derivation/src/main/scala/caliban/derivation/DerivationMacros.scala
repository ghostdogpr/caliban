package caliban.derivation

import caliban.schema.Annotations.GQLExclude

import scala.reflect.macros.blackbox

/**
 * Macro bundle for deriving instances of Schema for concrete classes,
 * representing public members as either regular fields or functions, depending on whether or not
 * the member takes parameters.
 */
class DerivationMacros(val c: blackbox.Context) extends CalibanUtils {
  import c.universe._

  private case class DeriveMember(sym: TermSymbol, envType: Type) {
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
    val detectedEnvType: Type   = detectEnvironment(scalaType)

    private val info: GraphQLInfo = GraphQLInfo(sym)

    private val fieldName: TermName = sym.name.normalize

    private val fieldNameStr: Tree = mkConst(sym.name.normalizeString)

    // TODO: Figure out how to cleanly extract the default argument, if any
    def deriveParam: Tree = Schema.summon(envType, scalaType) { paramSchema =>
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

    def deriveField: Tree = Schema.summon(envType, scalaType) { schema =>
      val args =
        params.filterNot(_.isEmpty) match {
          case Some(ps) =>
            // Non-empty list of parameters
            val argTrees = ps.map(p => DeriveMember(p.asTerm, envType).deriveParam)
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

    def deriveStep(parent: Type, resolveValue: TermName): Tree = Schema.summon(envType, scalaType) { schema =>
      val emptyParamList: Boolean =
        params.exists(_.isEmpty)

      params.filterNot(_.isEmpty) match {
        case Some(ps) =>
          // Non-empty list of parameters

          // Generate a function from the InputValue's received to a Step for resolving the method call
          val functionStep = mkFunction() { args =>
            val paramNames = ps.map(p => c.freshName(p.name))

            val builtArgs =
              ps.zip(paramNames).map { case (p, name) =>
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

  private def deriveInputObject(info: GraphQLInfo, env: Type, inputs: List[TermSymbol]): Tree = {
    val fields =
      inputs.map(i => DeriveMember(i, env).deriveParam)

    q"""
      ${refs.makeInputObject}(
        Some(${info.inputName}),
        ${info.description},
        List(..$fields)
      )
    """
  }

  private def deriveOutputObject(info: GraphQLInfo, env: Type, outputs: List[TermSymbol]): Tree = {
    val fields =
      outputs.map(o => DeriveMember(o, env).deriveField)

    q"""
      ${refs.makeObject}(
        Some(${info.name}),
        ${info.description},
        List(..$fields),
        ${info.directives}
      )
    """
  }

  private def deriveInterface(
    info: GraphQLInfo,
    env: Type,
    outputs: List[TermSymbol],
    subtypes: Map[Symbol, (List[TermSymbol], List[TermSymbol])]
  ): Tree = {
    val fields =
      outputs.map(o => DeriveMember(o, env).deriveField)

    val subtypeValues =
      subtypes.map { case (subtype, (ins, outs)) =>
        val subtypeInfo = GraphQLInfo(subtype)
        deriveOutputObject(subtypeInfo, env, outs)
      }

    val iface = TermName(c.freshName())
    q"""
        {
          lazy val $iface: ${typeRefs.__Type} = ${refs.makeInterface}(
            Some(${info.name}),
            ${info.description},
            () => List(..$fields),
            List(..$subtypeValues).map(_.copy(interfaces = () => Some(List($iface))))
          )
          $iface
        }
    """
  }

  private def deriveUnion(
    info: GraphQLInfo,
    env: Type,
    subtypes: Map[Symbol, (List[TermSymbol], List[TermSymbol])]
  ): Tree = {
    val subtypeValues =
      subtypes.map { case (subtype, (_, outs)) =>
        val subtypeInfo = GraphQLInfo(subtype)
        deriveOutputObject(subtypeInfo, env, outs)
      }

    val iface = TermName(c.freshName())
    q"""
        {
          lazy val $iface: ${typeRefs.__Type} = ${refs.makeUnion}(
            Some(${info.name}),
            ${info.description},
            List(..$subtypeValues)
          )
          $iface
        }
    """
  }

  private def deriveEnum(
    info: GraphQLInfo,
    subtypes: Set[Symbol]
  ): Tree = {
    val enumValues =
      subtypes.map { subtype =>
        val subtypeInfo = GraphQLInfo(subtype)
        q"""
            _root_.caliban.introspection.adt.__EnumValue(
              name = ${subtypeInfo.name},
              description = ${subtypeInfo.description},
              isDeprecated = ${subtypeInfo.isDeprecated},
              deprecationReason = ${subtypeInfo.deprecationReason}
            )
         """
      }

    q"""
          ${refs.makeEnum}(
            Some(${info.name}),
            ${info.description},
            List(..$enumValues),
            None
          )
    """
  }

  private def deriveStep(
    parent: Type,
    info: GraphQLInfo,
    env: Type,
    resolveValue: TermName,
    outputs: List[TermSymbol]
  ): Tree = {
    val fields =
      outputs.map(o => DeriveMember(o, env).deriveStepWithName(parent, resolveValue))

    q"""
      ${refs.ObjectStep}.apply[$env](
        ${info.name},
        Map(..$fields)
      )
    """
  }

  private def mergeEnvironments(members: List[TermSymbol]): Type = {
    val envTypes = members.map(m => DeriveMember(m, typeRefs.Any).envType).toSet - typeRefs.Any
    if (envTypes.nonEmpty) {
      envTypes.reduce((a, b) => tq"$a with $b".tpe)
    } else {
      typeRefs.Any
    }
  }

  private def deriveProductSchema(tpe: Type, requestedEnv: Type): Tree = {
    val inputs  = inputFields(tpe)
    val outputs = outputFields(tpe)

    val envType = mergeEnvironments(inputs ++ outputs)

    if (!(requestedEnv <:< requestedEnv)) {
      c.warning(
        c.enclosingPosition,
        s"deriveSchema was called with environment ${requestedEnv} but there are members in the derived type is using ${envType}!"
      )
    }

    val resolveValue = TermName(c.freshName())

    val info    = GraphQLInfo(tpe.typeSymbol)
    val toType  =
      q"""
        override def toType(isInput: Boolean = false, isSubscription: Boolean = false): ${typeRefs.__Type} =
          if (isInput) ${deriveInputObject(info, requestedEnv, inputs)}
          else ${deriveOutputObject(info, requestedEnv, outputs)}
      """
    val resolve =
      q"""
         override def resolve($resolveValue: $tpe): ${Step(requestedEnv)} =
           ${deriveStep(tpe, info, requestedEnv, resolveValue, outputs)}
      """

    val result = q"""
      new ${Schema(requestedEnv, tpe)} {
        $toType
        $resolve
      }
    """

    //    println(result)
    result
  }

  def deriveSumSchema(tpe: Type, requestedEnv: Type): Tree = {
    // NOTE: Sealed trait cannot have inputs
    val outputs = outputFields(tpe)

    println(s"Sealed trait outputs: $outputs")

    val subclasses = knownSubclassesOf(tpe.typeSymbol.asClass)
    println(s"Subclasses: $subclasses")

    val subclassInOut =
      subclasses.map { subclass =>
        val inputs  = inputFields(subclass.typeSignature)
        val outputs = outputFields(subclass.typeSignature)
        println(s"${subclass.name} trait inputs: $inputs")
        println(s"${subclass.name} trait outputs: $outputs")

        (subclass, (inputs, outputs))
      }.toMap

    val isEnum      = outputs.isEmpty && subclassInOut.forall { case (_, (in, out)) => in.isEmpty && out.isEmpty }
    val isUnion     = !isEnum && outputs.isEmpty
    val isInterface = !isEnum && !isUnion

    println(s"$isEnum / $isUnion / $isInterface")

    val info   = GraphQLInfo(tpe.typeSymbol)
    val toType =
      if (isInterface) {
        q"""
          override def toType(isInput: Boolean = false, isSubscription: Boolean = false): ${typeRefs.__Type} =
            ${deriveInterface(info, requestedEnv, outputs, subclassInOut)}
        """
      } else if (isUnion) {
        q"""
          override def toType(isInput: Boolean = false, isSubscription: Boolean = false): ${typeRefs.__Type} =
            ${deriveUnion(info, requestedEnv, subclassInOut)}
        """
      } else {
        q"""
           override def toType(isInput: Boolean = false, isSubscription: Boolean = false): ${typeRefs.__Type} =
             ${deriveEnum(info, subclasses)}
         """
      }

    val resolveValue = TermName(c.freshName())

    val cases   = subclassInOut.map { case (subclass, (ins, outs)) =>
      val n            = TermName(c.freshName())
      val subclassInfo = GraphQLInfo(subclass)
      cq"""$n: $subclass => ${deriveStep(subclass.typeSignature, subclassInfo, requestedEnv, n, outs)}"""
    }
    val resolve =
      q"""
         override def resolve($resolveValue: $tpe): ${Step(requestedEnv)} = {
           $resolveValue match {
             case ..$cases
           }
         }
      """

    val result = q"""
      new ${Schema(requestedEnv, tpe)} {
        $toType
        $resolve
      }
    """

    println(result)
    result
  }

  def deriveSchema[R, T](implicit wtr: WeakTypeTag[R], wtt: WeakTypeTag[T]): Tree = {
    val tpe          = wtt.tpe
    val requestedEnv = wtr.tpe

    val cls = if (tpe.typeSymbol.isClass) Some(tpe.typeSymbol.asClass) else None

    if (cls.exists(ct => ct.isCaseClass)) {
      deriveProductSchema(tpe, requestedEnv)
    } else if (cls.exists(ct => ct.isSealed && !ct.isJavaEnum)) {
      deriveSumSchema(tpe, requestedEnv)
    } else {
      c.abort(c.enclosingPosition, s"Only product and sum types can currently be derived with the `deriveSchema` macro")
    }
  }
}
