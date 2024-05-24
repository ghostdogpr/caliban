import caliban._
import caliban.federation.EntityResolver
import caliban.federation.tracing.ApolloFederatedTracing
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }
import caliban.schema.Annotations.GQLDeprecated
import caliban.schema.{ GenericSchema, Schema }
import models._
import services.{ InventoryService, ProductService, UserService }
import zio.query.ZQuery
import zio.{ URIO, ZIO }

case class Query(
  product: QueryProductArgs => URIO[ProductService, Option[models.Product]],
  @GQLDeprecated("Use product query instead") deprecatedProduct: DeprecatedProductArgs => URIO[
    ProductService,
    Option[DeprecatedProduct]
  ]
)

object Query {
  object apiSchema extends GenericSchema[ProductService with UserService]
  implicit val schema: Schema[ProductService with UserService, Query] = apiSchema.gen
}

object ProductSchema extends GenericSchema[ProductService with UserService] {
  val productResolver: EntityResolver[ProductService with UserService] =
    EntityResolver[ProductService with UserService, ProductArgs, models.Product] {
      case ProductArgs.IdOnly(id)                        =>
        ZQuery.serviceWithZIO[ProductService](_.getProductById(id.id))
      case ProductArgs.SkuAndPackage(sku, p)             =>
        ZQuery.serviceWithZIO[ProductService](_.getProductBySkuAndPackage(sku, p))
      case ProductArgs.SkuAndVariationId(sku, variation) =>
        ZQuery.serviceWithZIO[ProductService](_.getProductBySkuAndVariationId(sku, variation.id.id))
    }

  val userResolver: EntityResolver[UserService with ProductService] =
    EntityResolver[UserService with ProductService, UserArgs, User] { args =>
      ZQuery.serviceWithZIO[UserService](_.getUser)
    }

  val productResearchResolver: EntityResolver[UserService with ProductService] =
    EntityResolver.from[ProductResearchArgs] { args =>
      ZQuery.some(
        ProductResearch(
          CaseStudy(caseNumber = args.study.caseNumber, Some("Federation Study")),
          None
        )
      )
    }

  val deprecatedProductResolver: EntityResolver[ProductService with UserService] =
    EntityResolver[ProductService with UserService, DeprecatedProductArgs, DeprecatedProduct] { args =>
      ZQuery.some(
        models.DeprecatedProduct(
          sku = "apollo-federation-v1",
          `package` = "@apollo/federation-v1",
          reason = Some("Migrate to Federation V2"),
          createdBy = ZIO.serviceWithZIO[UserService](_.getUser)
        )
      )
    }

  val inventoryResolver: EntityResolver[InventoryService with UserService] =
    EntityResolver[InventoryService with UserService, InventoryArgs, Inventory] { args =>
      ZQuery.serviceWith[InventoryService](_.getById(args.id.id))
    }

  val api: GraphQL[ProductService with UserService with InventoryService] =
    graphQL(
      RootResolver(
        Query(
          args => ZIO.serviceWithZIO[ProductService](_.getProductById(args.id.id)),
          args =>
            ZIO.some(
              models.DeprecatedProduct(
                sku = "apollo-federation-v1",
                `package` = "@apollo/federation-v1",
                reason = Some("Migrate to Federation V2"),
                createdBy = ZIO.serviceWithZIO[UserService](_.getUser)
              )
            )
        )
      ),
      directives = List(
        __Directive(
          "custom",
          None,
          Set(__DirectiveLocation.OBJECT),
          _ => Nil,
          isRepeatable = false
        )
      )
    ) @@ federated(
      productResolver,
      userResolver,
      productResearchResolver,
      deprecatedProductResolver,
      inventoryResolver
    ) @@ ApolloFederatedTracing.wrapper()

  val print = api.render

}
