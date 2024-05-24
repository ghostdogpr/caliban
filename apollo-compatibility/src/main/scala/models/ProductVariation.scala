package models

import caliban.schema.Schema

case class ProductVariation(
  id: ID
)

object ProductVariation {
  implicit val schema: Schema[Any, ProductVariation] = Schema.gen
}
