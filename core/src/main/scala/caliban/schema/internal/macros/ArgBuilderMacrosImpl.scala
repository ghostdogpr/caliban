package caliban.schema.internal.macros

import caliban.schema.Annotations.{ GQLOneOfInput, GQLValueType }
import caliban.schema.{ ArgBuilder, OneOfArgBuilder, ProductArgBuilder, SingletonArgBuilder, SumArgBuilder }
import hearth.MacroCommons
import hearth.fp.effect._
import hearth.fp.instances._
import hearth.fp.syntax._
import hearth.std.StdExtensions

private[schema] trait ArgBuilderMacrosImpl { this: MacroCommons with StdExtensions with ArgBuilderAnnotationSupport =>

  private type Attempt[A] = MIO[Option[Expr[ArgBuilder[A]]]]

  protected def summonArgBuilderExpr[A: Type](excluded: UntypedMethod*): Either[String, Expr[ArgBuilder[A]]]

  private object Types {
    def OptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]
    def ListCtor: Type.Ctor1[List]     = Type.Ctor1.of[List]
    def MapCtor: Type.Ctor2[Map]       = Type.Ctor2.of[Map]
    val AnyType: Type[Any]             = Type.of[Any]
    val ListAnyType: Type[List[Any]]   = Type.of[List[Any]]
    val StringType: Type[String]       = Type.of[String]
    val ArrayAnyType: Type[Array[Any]] = Type.of[Array[Any]]
    val IntType: Type[Int]             = Type.of[Int]
    val GQLValueTypeType: Type[GQLValueType] = Type.of[GQLValueType]
    val GQLOneOfInputType: Type[GQLOneOfInput] = Type.of[GQLOneOfInput]

    def ArgBuilderOf[A: Type]: Type[ArgBuilder[A]] = Type.of[ArgBuilder[A]]
  }

  final class DerivationCtx[A](
    val tpe: Type[A],
    val cache: MLocal[ValDefsCache],
    val derivedType: Option[??]
  ) {

    def nest[B: Type]: DerivationCtx[B] =
      new DerivationCtx[B](Type[B], cache, derivedType)

    def getCached[B: Type]: MIO[Option[Expr[ArgBuilder[B]]]] = {
      implicit val argBuilderB: Type[ArgBuilder[B]] = Types.ArgBuilderOf[B]
      cache.get0Ary[ArgBuilder[B]]("cached-arg-builder-instance")
    }

    def cacheBuilt[B: Type](instance: Expr[ArgBuilder[B]]): MIO[Expr[ArgBuilder[B]]] = {
      implicit val argBuilderB: Type[ArgBuilder[B]] = Types.ArgBuilderOf[B]
      getCached[B].flatMap {
        case Some(ref) => MIO.pure(ref)
        case None      =>
          cache.buildCachedWith(
            "cached-arg-builder-instance",
            ValDefBuilder.ofLazy[ArgBuilder[B]](s"argBuilder_${Type[B].shortName}")
          )(_ => instance) >>
            getCached[B].map(_.getOrElse(instance))
      }
    }

    def forwardDeclareAndCache[B: Type](instance: => MIO[Expr[ArgBuilder[B]]]): MIO[Expr[ArgBuilder[B]]] = {
      implicit val argBuilderB: Type[ArgBuilder[B]] = Types.ArgBuilderOf[B]
      val builder                                   = ValDefBuilder.ofDef0[ArgBuilder[B]](s"argBuilder_${Type[B].shortName}")

      getCached[B].flatMap {
        case Some(ref) => MIO.pure(ref)
        case None      =>
          cache.forwardDeclare("cached-arg-builder-instance", builder) >>
            instance.flatMap { derived =>
              cache.buildCachedWith("cached-arg-builder-instance", builder)(_ => derived) >>
                getCached[B].map(_.getOrElse(derived))
            }
      }
    }
  }

  object DerivationCtx {
    def from[A: Type](derivedType: Option[??]): DerivationCtx[A] =
      new DerivationCtx(Type[A], ValDefsCache.mlocal, derivedType)
  }

  def deriveTypeClass[A: Type]: Expr[ArgBuilder[A]] =
    deriveOrFail[A]("ArgBuilder.derived", Some(Type[A].as_??))

  private def deriveOrFail[A: Type](macroName: String, derivedType: Option[??]): Expr[ArgBuilder[A]] =
    Log
      .namedScope(s"Deriving ${Types.ArgBuilderOf[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val ctx = DerivationCtx.from[A](derivedType)
          runSafe {
            for {
              _      <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveRecursively[A](ctx)
              cache  <- ctx.cache.get
            } yield cache.toValDefs.use(_ => result)
          }
        }
      }
      .runToExprOrFail(macroName, infoRendering = DontRender) { (_, errors) =>
        errors.iterator.map(_.getMessage).mkString("\n")
      }

  private lazy val ignoredImplicits: Seq[UntypedMethod] =
    Type.of[ArgBuilder.type].methods.collect {
      case method
          if method.value.isImplicit &&
            Set("derived", "autoDerived", "genAuto").contains(method.value.name) =>
        method.value.asUntyped
    }

  private def deriveRecursively[A: Type](ctx: DerivationCtx[A]): MIO[Expr[ArgBuilder[A]]] =
    MIO.scoped { runSafe =>
      runSafe {
        attemptUsingCached[A](ctx)
      } orElse runSafe {
        attemptUsingImplicit[A](ctx)
      } orElse runSafe {
        attemptAsOption[A](ctx)
      } orElse runSafe {
        attemptAsList[A](ctx)
      } orElse runSafe {
        attemptAsMap[A](ctx)
      } orElse runSafe {
        attemptAsSingleton[A](ctx)
      } orElse runSafe {
        attemptAsCaseClass[A](ctx)
      } orElse runSafe {
        attemptAsEnum[A](ctx)
      } getOrElse runSafe {
        MIO.fail(new IllegalArgumentException(s"Cannot derive ArgBuilder for ${Type[A].prettyPrint}"))
      }
    }

  private def attemptUsingCached[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    ctx.getCached[A]

  private def attemptUsingImplicit[A: Type](ctx: DerivationCtx[A]): Attempt[A] = {
    implicit val argBuilderA: Type[ArgBuilder[A]] = Types.ArgBuilderOf[A]

    if (ctx.derivedType.exists(_.Underlying =:= Type[A])) MIO.pure(None)
    else
      summonArgBuilderExpr[A](ignoredImplicits: _*) match {
        case Right(instance) => ctx.cacheBuilt[A](instance).map(Some(_))
        case Left(_)         => MIO.pure(None)
      }
  }

  private def attemptAsOption[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    Type[A] match {
      case Types.OptionCtor(inner) =>
        import inner.{ Underlying => Inner }
        deriveRecursively[Inner](ctx.nest[Inner])
          .flatMap { innerBuilder =>
            ctx.cacheBuilt[A](
              Expr.quote(ArgBuilder.option[Inner](Expr.splice(innerBuilder))).asInstanceOf[Expr[ArgBuilder[A]]]
            )
          }
          .map(Some(_))
      case _                       => MIO.pure(None)
    }

  private def attemptAsList[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    Type[A] match {
      case Types.ListCtor(inner) =>
        import inner.{ Underlying => Inner }
        deriveRecursively[Inner](ctx.nest[Inner])
          .flatMap { innerBuilder =>
            ctx.cacheBuilt[A](
              Expr.quote(ArgBuilder.list[Inner](Expr.splice(innerBuilder))).asInstanceOf[Expr[ArgBuilder[A]]]
            )
          }
          .map(Some(_))
      case _                     => MIO.pure(None)
    }

  private def attemptAsMap[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    Type[A] match {
      case Types.MapCtor(key, value) =>
        import key.{ Underlying => Key }
        import value.{ Underlying => Value }
        for {
          keyBuilder   <- deriveRecursively[Key](ctx.nest[Key])
          valueBuilder <- deriveRecursively[Value](ctx.nest[Value])
          cached       <- ctx.cacheBuilt[A](
                            Expr.quote(ArgBuilder.map[Key, Value](Expr.splice(keyBuilder), Expr.splice(valueBuilder)))
                              .asInstanceOf[Expr[ArgBuilder[A]]]
                          )
        } yield Some(cached)
      case _                         => MIO.pure(None)
    }

  private def attemptAsSingleton[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    SingletonValue.parse[A].toOption match {
      case Some(singleton) =>
        val annotations = typeAnnotationsExpr[A]
        ctx
          .cacheBuilt[A](
            Expr.quote(
              new SingletonArgBuilder[A](
                Expr.splice(singleton.singletonExpr),
                Expr.splice(Expr(Type[A].shortName)),
                Expr.splice(annotations)
              )
            )
          )
          .map(Some(_))
      case None            => MIO.pure(None)
    }

  private def attemptAsCaseClass[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    CaseClass.parse[A].toOption match {
      case Some(caseClass) =>
        ctx
          .forwardDeclareAndCache[A] {
            val fields = caseClass.primaryConstructor.parameters.flatten.toList
            for {
              fieldExprs <- fields.traverse { case (name, param) =>
                              import param.tpe.{ Underlying => Field }
                              deriveRecursively[Field](ctx.nest[Field]).map { builder =>
                                  val annotations = paramAnnotationsExpr(param)
                                  Expr.quote(
                                    (
                                      Expr.splice(Expr(name)),
                                      Expr.splice(annotations),
                                      Expr.splice(builder).asInstanceOf[ArgBuilder[Any]]
                                    )
                                  )
                                }
                            }
              fieldsExpr = fieldExprs.foldRight(Expr.quote(List.empty[(String, List[Any], ArgBuilder[Any])]): Expr[
                List[(String, List[Any], ArgBuilder[Any])]
              ]) { (fieldExpr, acc) =>
                Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
              }
              isValueType: Boolean = fields.nonEmpty && (Type[A] <:< Type.of[AnyVal] || {
                implicit val gqlValueTypeT: Type[GQLValueType] = Types.GQLValueTypeType
                hasTypeAnnotation[A, GQLValueType]
              })
              constructor <- buildConstructor[A](caseClass)
            } yield Expr.quote(
              new ProductArgBuilder[A](Expr.splice(fieldsExpr), Expr.splice(Expr(isValueType)), Expr.splice(constructor))
            )
          }
          .map(Some(_))
      case None            => MIO.pure(None)
    }

  private def buildConstructor[A: Type](caseClass: CaseClass[A]): MIO[Expr[Array[Any] => A]] = {
    implicit val arrayAny: Type[Array[Any]] = Types.ArrayAnyType
    implicit val anyType: Type[Any]         = Types.AnyType
    implicit val intType: Type[Int]         = Types.IntType

    LambdaBuilder
      .of1[Array[Any]]("values")
      .traverse { valuesExpr =>
        val fields = caseClass.primaryConstructor.parameters.flatten.toList.map { case (name, param) =>
          import param.tpe.{ Underlying => Field }
          val typedExpr = Expr.quote(
            Expr.splice(valuesExpr)(Expr.splice(Expr(param.index))).asInstanceOf[Field]
          )
          name          -> typedExpr.as_??
        }.toMap

        caseClass.primaryConstructor(fields) match {
          case Right(constructExpr) => MIO.pure(constructExpr)
          case Left(error)          => MIO.fail(new IllegalStateException(error))
        }
      }
      .map(_.build[A])
  }

  private def attemptAsEnum[A: Type](ctx: DerivationCtx[A]): Attempt[A] =
    Enum.parse[A].toOption match {
      case Some(enumm) =>
        ctx
          .forwardDeclareAndCache[A] {
            for {
              subTypeExprs <- enumm.directChildren.toList.traverse { case (label, child) =>
                                import child.{ Underlying => Child }
                                deriveRecursively[Child](ctx.nest[Child]).map { builder =>
                                    val annotations = typeAnnotationsExpr[Child]
                                    Expr.quote(
                                      (
                                        Expr.splice(Expr(label)),
                                        Expr.splice(annotations),
                                        Expr.splice(builder).asInstanceOf[ArgBuilder[Any]]
                                      )
                                    )
                                }
                              }
              subTypesExpr = subTypeExprs.foldRight(Expr.quote(List.empty[(String, List[Any], ArgBuilder[Any])]): Expr[
                List[(String, List[Any], ArgBuilder[Any])]
              ]) { (subTypeExpr, acc) =>
                Expr.quote(Expr.splice(subTypeExpr) :: Expr.splice(acc))
              }
              isOneOf: Boolean = {
                implicit val gqlOneOfInputT: Type[GQLOneOfInput] = Types.GQLOneOfInputType
                hasTypeAnnotation[A, GQLOneOfInput]
              }
            } yield {
              if (isOneOf)
                Expr.quote(new OneOfArgBuilder[A](Expr.splice(subTypesExpr), Expr.splice(Expr(Type[A].shortName))))
              else
                Expr.quote(new SumArgBuilder[A](Expr.splice(subTypesExpr), Expr.splice(Expr(Type[A].shortName))))
            }
          }
          .map(Some(_))
      case None        => MIO.pure(None)
    }
}
