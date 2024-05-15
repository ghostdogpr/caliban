package models

import caliban.schema.{ ArgBuilder, Schema }

case class DeprecatedProductArgs(
  sku: String,
  `package`: String
)

object DeprecatedProductArgs {
  implicit val schema: Schema[Any, DeprecatedProductArgs]    = Schema.gen
  implicit val argBuilder: ArgBuilder[DeprecatedProductArgs] = ArgBuilder.gen[DeprecatedProductArgs]
}
