package caliban.relay

import caliban.CalibanError.ExecutionError

/**
 * Used in GlobalIds to resolve the type of the returned object based on the ID.
 * This is needed in order to correctly derive the subtype of the object.
 */
trait TypeResolver[ID] {
  def resolve(a: ID): Either[ExecutionError, String]
}
