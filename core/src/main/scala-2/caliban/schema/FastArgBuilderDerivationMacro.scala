package caliban.schema

import scala.reflect.macros.blackbox

private class FastArgBuilderDerivationMacro(val c: blackbox.Context) {

  import c.universe._
  import ScalaTypes._
  import CalibanTypes._

  def materialize[T: c.WeakTypeTag]: c.Expr[caliban.schema.ArgBuilder[T]] = {
    val tpe   = weakTypeOf[T]
    val klass = tpe.typeSymbol.asClass

    val isOneOfInput  = klass.annotations.exists(_.tree.tpe =:= GQLOneOfInputType)
    val isSealedTrait = klass.isSealed && klass.isTrait

    // order matters
    if (isSealedTrait && isOneOfInput)
      materializeOneOfInput[T]
    else if (isSealedTrait)
      materializeEnum[T]
    else if (klass.isClass)
      materializeClass[T]
    else
      c.abort(c.enclosingPosition, s"Cannot generate ArgBuilder for type $tpe")
  }

  private def materializeClass[T: c.WeakTypeTag]: c.Expr[caliban.schema.ArgBuilder[T]] = {
    val tpe   = weakTypeOf[T]
    val klass = tpe.typeSymbol.asClass

    if (Utils.isValueType(klass))
      materializeValueType[T]
    else
      materializeGqlObject[T]
  }

  private def materializeEnum[T: c.WeakTypeTag]: c.Expr[caliban.schema.ArgBuilder[T]] = {
    val tpe      = weakTypeOf[T]
    val klass    = tpe.typeSymbol.asClass
    val subtypes = klass.knownDirectSubclasses.map { s =>
      val gqlName = s.annotations.collectFirst { ann =>
        ann.tree match { case Apply(_, List(Literal(Constant(s: String)))) if ann.tree.tpe =:= GQLNameType => s }
      }

      val name = gqlName.getOrElse(s.name.toString)

      (name, s.asClass.module)
    }

    val cases = subtypes.map { case (name, sig) => cq"$name => new $RightSym($sig)" }

    val inputVarName = TermName("v")
    val resultType   = weakTypeOf[Either[caliban.CalibanError.ExecutionError, T]]

    c.Expr[caliban.schema.ArgBuilder[T]](q"""
        new $ArgBuilderSym[$tpe] {
          private def parseEnum(c: String) = c match {
            case ..$cases
            case r => ${makeError(tpe, s"${getTypePrettyName(tpe)}: unknown variant $${r}")}
          }

          final def build($inputVarName: $InputValueType): $resultType =
            $inputVarName match {
              case $StringValueObj(c) => parseEnum(c)
              case $EnumValueObj(c) => parseEnum(c)
              case _ => ${makeError(tpe, s"${getTypePrettyName(tpe)}: expected enum")}
            }
        }
       """)
  }

  private def materializeOneOfInput[T: c.WeakTypeTag]: c.Expr[caliban.schema.ArgBuilder[T]] = {
    val tpe   = weakTypeOf[T]
    val klass = tpe.typeSymbol.asClass

    val resultType = weakTypeOf[Either[caliban.CalibanError.ExecutionError, T]]
    val objVarName = TermName("c")
    val subtypes   = klass.knownDirectSubclasses.zipWithIndex.map { case (s, i) =>
      val sType          = s.asType.toType
      val argBuilderName = TermName(s"argBuilder$$$i")
      val instanceDef    =
        q"private[this] lazy val $argBuilderName: $ArgBuilderSym[$s] = ${getImplicitArgBuilder(sType)}"

      val builder = q"$argBuilderName.build($objVarName).asInstanceOf[$resultType]"
      (instanceDef, builder)
    }.toList

    val instanceDefs = subtypes.map(_._1)

    val builder = subtypes.map(_._2) match {
      case head :: tail =>
        val t       = tail.foldLeft(head) { case (acc, item) => q"$acc.orElse($item)" }
        val default = makeError(tpe, s"${getTypePrettyName(tpe)}: unexpected case")
        q"$t.orElse($default)"
      case Nil          => c.abort(c.enclosingPosition, s"$tpe is marked as @oneOf, but doesn't have subtypes")
    }

    val oneSizeError   = makeError(tpe, s"${getTypePrettyName(tpe)}: expected object of size 1")
    val notObjectError = makeError(tpe, s"${getTypePrettyName(tpe)}: expected object")

    val inputVarName = TermName("v")

    c.Expr[caliban.schema.ArgBuilder[T]](q"""
        new $ArgBuilderSym[$tpe] {
           ..$instanceDefs

          final def build($inputVarName: $InputValueType): $resultType =
            $inputVarName match {
              case $objVarName @ $ObjectValueObj(m) if m.size == 1 => $builder
              case $ObjectValueObj(_) => $oneSizeError
              case _ => $notObjectError
            }
        }
       """)
  }

  private def materializeValueType[T: c.WeakTypeTag] = {
    val tpe    = weakTypeOf[T]
    val klass  = tpe.typeSymbol.asClass
    val params = klass.primaryConstructor.asMethod.paramLists.flatten

    val fields = params.zipWithIndex.map { case (v, i) => buildParam(v, i) }
    fields match {
      case List(field) => c.Expr[caliban.schema.ArgBuilder[T]](q"${field.implicitArgBuilder}.map(r => new $tpe(r))")
      case _           => c.abort(klass.pos, s"$tpe is not value type")
    }
  }

  private def materializeGqlObject[T: c.WeakTypeTag] = {
    val tpe    = weakTypeOf[T]
    val klass  = tpe.typeSymbol.asClass
    val params = klass.primaryConstructor.asMethod.paramLists.flatten

    val fields = params.zipWithIndex.map { case (v, i) => buildParam(v, i) }

    val objVarName = TermName("c")

    val last =
      q"new $RightSym[$ExecutionErrorType, $tpe](new $klass(..${fields.map(_.resName)}))"

    // `if (tmp.isRight) ... else ...` is used instead of pattern matching because the former is much faster
    val result: Tree = fields.foldRight(last) { case (field, acc) =>
      q"""
         {
          val ${field.tempName}: $EitherSym[$ExecutionErrorType, ${field.theType}] =
            ${field.argBuilderName}.build($objVarName.getOrElse(${field.name}, ${field.defaultValue}))

          if (${field.tempName}.isRight) {
            val ${field.resName} = ${field.tempName}.asInstanceOf[$RightType[$ExecutionErrorType, ${field.theType}]].value
            $acc
          } else ${field.tempName}.asInstanceOf[$EitherSym[$ExecutionErrorType, $tpe]]
         }
       """
    }

    val instanceDefs = fields.map(field =>
      q"private[this] lazy val ${field.argBuilderName}: $ArgBuilderSym[${field.theType}] = ${field.implicitArgBuilder}"
    )

    val inputVarName = TermName("v")
    val resultType   = weakTypeOf[Either[caliban.CalibanError.ExecutionError, T]]

    c.Expr[caliban.schema.ArgBuilder[T]](q"""
        new $ArgBuilderSym[$tpe] {
          ..$instanceDefs

          final def build($inputVarName: $InputValueType): $resultType =
            $inputVarName match {
              case ${ObjectValueSym.companion}($objVarName) => $result
              case _ => ${makeError(tpe, s"${getTypePrettyName(tpe)}: expected object")}
            }
        }
       """)
  }

  private def buildParam(param: Symbol, ix: Long) = {
    var name                             = param.name.toString
    var defaultValue: caliban.InputValue = caliban.Value.NullValue

    param.annotations.foreach { ann =>
      ann.tree match {
        case Apply(_, List(Literal(Constant(s: String)))) if ann.tree.tpe =:= GQLNameType    =>
          name = s
        case Apply(_, List(Literal(Constant(s: String)))) if ann.tree.tpe =:= GQLDefaultType =>
          caliban.parsing.Parser.parseInputValue(s) match {
            case Left(value)  =>
              c.abort(c.enclosingPosition, s"Cannot parse $GQLDefaultType annotation body")
            case Right(value) =>
              defaultValue = value
          }
        case _                                                                               =>
      }
    }

    new CaseClassParam(ix, name, defaultValue, param)
  }

  private class CaseClassParam(
    val ix: Long,
    val name: String,
    val defaultValue: caliban.InputValue,
    val theType: Symbol
  ) {

    val argBuilderName: TermName = TermName(s"argBuilderInstance$$$ix")
    val resName: TermName        = TermName(s"res$$$ix")
    val tempName: TermName       = TermName(s"temp$$$ix")

    def implicitArgBuilder: Tree = getImplicitArgBuilder(theType.typeSignature)
  }

  private def getImplicitArgBuilder(tpe: Type): Tree = {
    val tc = ArgBuilderTC

    c.inferImplicitValue(appliedType(tc, List(tpe))) match {
      case EmptyTree => c.abort(c.enclosingPosition, s"Could not find $tc instance for type $tpe")
      case x         => x
    }
  }

  private def getTypePrettyName(tpe: Type) = tpe.typeSymbol.name.decodedName.toString

  private object Utils {

    def isValueType(klass: ClassSymbol): Boolean = {
      val isValueType = klass.annotations.exists(_.tree.tpe =:= GQLValueTypeType)
      klass.isDerivedValueClass || isValueType
    }

  }

  private object CalibanTypes {
    val GQLDefaultType: Type    = typeOf[caliban.schema.Annotations.GQLDefault]
    val GQLNameType: Type       = typeOf[caliban.schema.Annotations.GQLName]
    val GQLOneOfInputType: Type = typeOf[caliban.schema.Annotations.GQLOneOfInput]
    val GQLValueTypeType: Type  = typeOf[caliban.schema.Annotations.GQLValueType]

    val InputValueType: Type     = typeOf[caliban.InputValue]
    val ExecutionErrorType: Type = typeOf[caliban.CalibanError.ExecutionError]

    val ObjectValueSym: Symbol = symbolOf[caliban.InputValue.ObjectValue]
    val ObjectValueObj: Symbol = ObjectValueSym.companion

    val NullValueSym: Symbol = symbolOf[caliban.Value.NullValue.type].asClass.module

    val IntNumberObj: Symbol = symbolOf[caliban.Value.IntValue.IntNumber].companion

    val LongNumberObj: Symbol = symbolOf[caliban.Value.IntValue.LongNumber].companion

    val FloatNumberObj: Symbol = symbolOf[caliban.Value.FloatValue.FloatNumber].companion

    val DoubleNumberObj: Symbol = symbolOf[caliban.Value.FloatValue.DoubleNumber].companion

    val BigDecimalNumberObj: Symbol = symbolOf[caliban.Value.FloatValue.BigDecimalNumber].companion

    val BigIntNumberObj: Symbol = symbolOf[caliban.Value.IntValue.BigIntNumber].companion

    val BooleanValueObj: Symbol = symbolOf[caliban.Value.BooleanValue].companion

    val StringValueObj: Symbol = symbolOf[caliban.Value.StringValue].companion

    val EnumValueObj: Symbol = symbolOf[caliban.Value.EnumValue].companion

    val VariableValueObj: Symbol = symbolOf[caliban.InputValue.VariableValue].companion

    val ListValueObj: Symbol = symbolOf[caliban.InputValue.ListValue].companion

    val ArgBuilderTC: Type    = typeOf[caliban.schema.ArgBuilder[_]].typeConstructor
    val ArgBuilderSym: Symbol = ArgBuilderTC.typeSymbol

    val ExecutionErrorObj: Symbol = ExecutionErrorType.typeSymbol.companion
  }

  private object ScalaTypes {
    val EitherSym: Symbol = symbolOf[scala.Either[_, _]].toTypeConstructor.typeSymbol

    val LeftSym: Symbol = symbolOf[scala.Left[_, _]].toTypeConstructor.typeSymbol

    val RightType: TypeSymbol = symbolOf[Right[_, _]]
    val RightSym: Symbol      = RightType.toTypeConstructor.typeSymbol

    val BigIntObj: Symbol = symbolOf[scala.math.BigInt].companion

    val BigDecimalObj: Symbol = symbolOf[scala.math.BigDecimal].companion
  }

  private def makeError(tpe: Type, msg: String) =
    q"new ${ScalaTypes.LeftSym}[$ExecutionErrorType, $tpe]($ExecutionErrorObj($msg))"

  implicit val inputValueLiftable: Liftable[caliban.InputValue] = new Liftable[caliban.InputValue] {

    override def apply(value: caliban.InputValue): c.universe.Tree = value match {
      case caliban.Value.NullValue                      => q"$NullValueSym"
      case caliban.Value.IntValue.IntNumber(i)          => q"$IntNumberObj($i)"
      case caliban.Value.IntValue.LongNumber(i)         => q"$LongNumberObj($i)"
      case caliban.Value.IntValue.BigIntNumber(i)       => q"$BigIntNumberObj($BigIntObj(${i.toString}))"
      case caliban.Value.FloatValue.FloatNumber(i)      => q"$FloatNumberObj($i)"
      case caliban.Value.FloatValue.DoubleNumber(i)     => q"$DoubleNumberObj($i)"
      case caliban.Value.FloatValue.BigDecimalNumber(i) => q"$BigDecimalNumberObj($BigDecimalObj(${i.toString}))"
      case caliban.Value.StringValue(i)                 => q"$StringValueObj($i)"
      case caliban.Value.BooleanValue(i)                => q"$BooleanValueObj($i)"
      case caliban.Value.EnumValue(i)                   => q"$EnumValueObj($i)"
      case caliban.InputValue.VariableValue(i)          => q"$VariableValueObj($i)"
      case caliban.InputValue.ListValue(lst)            => q"$ListValueObj(List(..${lst.map(apply)}))"
      case caliban.InputValue.ObjectValue(obj)          =>
        val ts = obj.map { case (k, v) => q"($k, ${apply(v)})" }
        q"$ObjectValueObj(Map(..$ts))"
    }
  }
}
