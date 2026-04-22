package caliban.schema

import caliban.schema.internal.macros.ArgBuilderMacros

import scala.language.experimental.macros

trait ArgBuilderDerivation {

  def derived[A]: ArgBuilder[A] = macro ArgBuilderMacros.deriveTypeClassImpl[A]

  @deprecated("ArgBuilder derivation is automatic by default; use ArgBuilder.derived only when you need explicit materialization.", "4.0.0")
  def gen[A]: ArgBuilder[A] = macro ArgBuilderMacros.deriveTypeClassImpl[A]
}

@deprecated("ArgBuilder derivation is automatic by default; imports are no longer required.", "4.0.0")
trait AutoArgBuilderDerivation extends ArgBuilderInstances

private[schema] trait LowPriorityDerivedArgBuilder {
  implicit def autoDerived[A]: ArgBuilder[A] = macro ArgBuilderMacros.deriveTypeClassImpl[A]

  @deprecated("ArgBuilder derivation is automatic by default; imports are no longer required.", "4.0.0")
  def genAuto[A]: ArgBuilder[A] = macro ArgBuilderMacros.deriveTypeClassImpl[A]
}
