package models

import caliban.schema.Schema

@GQLShareable
case class ProductDimension(
  size: Option[String],
  weight: Option[Float],
  @GQLInaccessible unit: Option[String]
)

object ProductDimension {
  implicit val schema: Schema[Any, ProductDimension] = Schema.gen
}
