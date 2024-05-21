package models

import caliban.InputValue
import caliban.schema.{ ArgBuilder, Schema }

sealed trait ProductArgs

object ProductArgs {
  case class IdOnly(id: ID)                                              extends ProductArgs
  case class SkuAndPackage(sku: String, `package`: String)               extends ProductArgs
  case class SkuAndVariationId(sku: String, variation: ProductVariation) extends ProductArgs

  private implicit val variationArgs: ArgBuilder[ProductVariation] = ArgBuilder.gen[ProductVariation]
  val idOnlyArgBuilder: ArgBuilder[IdOnly]                         = ArgBuilder.gen[IdOnly]
  val skuAndPackageArgBuilder: ArgBuilder[SkuAndPackage]           = ArgBuilder.gen[SkuAndPackage]
  val skuAndVariationIdArgBuilder: ArgBuilder[SkuAndVariationId]   = ArgBuilder.gen[SkuAndVariationId]

  implicit val argBuilder: ArgBuilder[ProductArgs] = (input: InputValue) =>
    (for {
      error <- skuAndVariationIdArgBuilder.build(input).swap
      _     <- skuAndPackageArgBuilder.build(input).swap
      _     <- idOnlyArgBuilder.build(input).swap
    } yield error).swap

  implicit val idOnlySchema: Schema[Any, ProductArgs.IdOnly]                       = Schema.gen
  implicit val skuAndPackageSchema: Schema[Any, ProductArgs.SkuAndPackage]         = Schema.gen
  implicit val skuAndVariationIdSchema: Schema[Any, ProductArgs.SkuAndVariationId] = Schema.gen

}
