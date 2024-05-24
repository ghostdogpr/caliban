package models

import caliban.schema.{ ArgBuilder, Schema }

case class InventoryArgs(id: ID)

object InventoryArgs {
  implicit val schema: Schema[Any, InventoryArgs]    = Schema.gen
  implicit val argBuilder: ArgBuilder[InventoryArgs] = ArgBuilder.gen
}
