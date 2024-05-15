package models

import caliban.schema.Schema
import zio.UIO

@GQLKey("id")
@GQLKey("sku package")
@GQLKey("sku variation { id }")
@Custom
case class Product(
  id: ID,
  sku: Option[String],
  `package`: Option[String],
  variation: Option[ProductVariation],
  dimensions: Option[ProductDimension],
  @GQLProvides("totalProductsCreated") createdBy: UIO[Option[User]],
  @GQLTag("internal") notes: Option[String],
  research: List[ProductResearch]
)

object Product {

  implicit val schema: Schema[Any, Product] = Schema.gen

}
