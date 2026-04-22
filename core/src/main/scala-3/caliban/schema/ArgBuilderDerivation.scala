package caliban.schema

import caliban.schema.internal.macros.ArgBuilderMacros

import scala.annotation.nowarn
import scala.quoted.*

trait ArgBuilderDerivation {

  inline def derived[A]: ArgBuilder[A] = ${ ArgBuilderMacros.deriveTypeClassImpl[A] }

  @deprecated("ArgBuilder derivation is automatic by default; use ArgBuilder.derived only when you need explicit materialization.", "4.0.0")
  inline def gen[A]: ArgBuilder[A] = derived[A]

  @deprecated("ArgBuilder derivation is automatic by default; `ArgBuilder` can be used directly in derives clauses.", "4.0.0")
  sealed abstract class Auto[A] extends ArgBuilder[A] {
    inline given genAuto[T]: ArgBuilder[T] = ArgBuilder.derived[T]
  }

  @deprecated("ArgBuilder derivation is automatic by default; `ArgBuilder` can be used directly in derives clauses.", "4.0.0")
  object Auto {
    @nowarn("msg=anonymous class")
    inline def derived[A]: Auto[A] = new {
      private val impl = ArgBuilder.derived[A]
      export impl.*
    }
  }

  @deprecated("ArgBuilder derivation is automatic by default; `ArgBuilder` can be used directly in derives clauses.", "4.0.0")
  final type GenAuto[A] = Auto[A]
}

@deprecated("ArgBuilder derivation is automatic by default; imports are no longer required.", "4.0.0")
trait AutoArgBuilderDerivation extends ArgBuilderInstances

private[schema] trait LowPriorityDerivedArgBuilder {
  inline implicit def autoDerived[A]: ArgBuilder[A] = ArgBuilder.derived[A]

  @deprecated("ArgBuilder derivation is automatic by default; imports are no longer required.", "4.0.0")
  inline def genAuto[A]: ArgBuilder[A] = autoDerived[A]
}
