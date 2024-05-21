package models

import caliban.schema.{ GenericSchema, Schema }
import services.{ InventoryService, UserService }

@GQLInterfaceObject
@GQLKey("email")
case class Inventory(
  id: ID,
  deprecatedProducts: List[DeprecatedProduct]
)

object Inventory {
  object genSchema extends GenericSchema[InventoryService with UserService]
  implicit val schema: Schema[InventoryService with UserService, Inventory] = genSchema.gen
}
