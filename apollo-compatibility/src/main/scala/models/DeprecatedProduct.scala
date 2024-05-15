package models

import caliban.schema.{ GenericSchema, Schema }
import services.UserService
import zio.URIO

@GQLKey("sku package")
case class DeprecatedProduct(
  sku: String,
  `package`: String,
  reason: Option[String],
  createdBy: URIO[UserService, Option[User]]
)

object DeprecatedProduct {
  object apiSchema extends GenericSchema[UserService]

  implicit val schema: Schema[UserService, DeprecatedProduct] = apiSchema.gen[UserService, DeprecatedProduct]
}
