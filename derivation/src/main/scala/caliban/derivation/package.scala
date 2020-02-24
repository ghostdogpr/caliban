package caliban

import caliban.schema.Schema

import scala.language.experimental.macros

package object derivation {
  def deriveSchemaInstance[T]: Schema[Any, T] = macro DerivationMacros.deriveSchema[T]
}
