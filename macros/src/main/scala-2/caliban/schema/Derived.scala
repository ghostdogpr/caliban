package caliban.schema

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Cannot find a ${T}.

Caliban derives a Schema automatically for basic Scala types, case classes and sealed traits, but
you need to manually provide an implicit Schema for other types that could be nested inside your type.
If you use a custom type as an argument, you also need to provide an implicit ArgBuilder for that type.
See https://ghostdogpr.github.io/caliban/docs/schema.html for more information.
"""
)
case class Derived[T](schema: T) extends AnyVal
