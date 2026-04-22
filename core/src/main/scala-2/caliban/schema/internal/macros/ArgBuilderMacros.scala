package caliban.schema.internal.macros

import caliban.schema.ArgBuilder
import hearth.MacroCommonsScala2

import scala.reflect.macros.blackbox

final private[schema] class ArgBuilderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ArgBuilderAnnotationSupportScala2
    with ArgBuilderMacrosImpl {

  override protected def summonArgBuilderExpr[A: Type](excluded: UntypedMethod*): Either[String, Expr[ArgBuilder[A]]] =
    Type[ArgBuilder[A]].summonExprIgnoring(excluded: _*).toEither

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[ArgBuilder[A]] =
    deriveTypeClass[A]
}
