package services

import zio.{ Ref, UIO, ZIO, ZLayer }

trait ProductService {
  def getProductById(id: String): UIO[Option[models.Product]]
  def getProductBySkuAndPackage(sku: String, pack: String): UIO[Option[models.Product]]
  def getProductBySkuAndVariationId(sku: String, variationId: String): UIO[Option[models.Product]]
}

object ProductService {
  val productsResearch = List(
    models.ProductResearch(
      study = models.CaseStudy(models.ID("1234"), Some("Federation Study")),
      outcome = None
    ),
    models.ProductResearch(
      study = models.CaseStudy(models.ID("1235"), Some("Studio Study")),
      outcome = None
    )
  )

  val inMemory: ZLayer[Any, Nothing, ProductService] =
    ZLayer(
      Ref
        .make(
          List(
            models.Product(
              id = models.ID("apollo-federation"),
              sku = Some("federation"),
              `package` = Some("@apollo/federation"),
              variation = Some(models.ProductVariation(models.ID("OSS"))),
              dimensions = Some(models.ProductDimension(Some("small"), Some(1.0f), Some("kg"))),
              createdBy = ZIO.some(
                models.User(
                  models.ID("support@apollographql.com"),
                  Some(1337),
                  Some("Jane Smith"),
                  averageProductsCreatedPerYear = Some(1337 / 10),
                  yearsOfEmployment = 10
                )
              ),
              notes = Some("This is a test product"),
              research = productsResearch.init
            ),
            models.Product(
              id = models.ID("apollo-studio"),
              sku = Some("studio"),
              `package` = Some(""),
              variation = Some(models.ProductVariation(models.ID("platform"))),
              dimensions = Some(models.ProductDimension(Some("small"), Some(1.0f), Some("kg"))),
              createdBy = ZIO.some(
                models.User(
                  models.ID("support@apollographql.com"),
                  Some(1337),
                  Some("Jane Smith"),
                  averageProductsCreatedPerYear = Some(1337 / 10),
                  yearsOfEmployment = 10
                )
              ),
              notes = Some("This is a note"),
              research = productsResearch.tail
            )
          )
        )
        .map { products =>
          new ProductService {
            override def getProductById(id: String): UIO[Option[models.Product]] =
              products.get.map(_.find(_.id.id == id))

            override def getProductBySkuAndPackage(sku: String, pack: String): UIO[Option[models.Product]] =
              products.get.map(_.find(p => p.sku.contains(sku) && p.`package`.contains(pack)))

            override def getProductBySkuAndVariationId(sku: String, variationId: String): UIO[Option[models.Product]] =
              products.get.map(
                _.find(p =>
                  p.sku.contains(sku) && p.variation.contains(models.ProductVariation(models.ID(variationId)))
                )
              )
          }
        }
    )
}
