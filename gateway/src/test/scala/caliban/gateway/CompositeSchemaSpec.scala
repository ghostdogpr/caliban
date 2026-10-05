package caliban.gateway

import caliban.federation.EntityResolver
import caliban.federation.v2_6.{ federated, GQLKey }
import caliban.gateway.CompositionDiagnostic.Code
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.SchemaCoordinate.Member
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.composition.DirectiveComposition.FieldCoordinate
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, RootResolver }
import zio._
import zio.http._
import zio.query.ZQuery
import zio.test._

object CompositeSchemaSpec extends ZIOSpecDefault {

  private val eShopNames = List("accounts", "inventory", "products", "reviews")

  private def eShopSchemas: UIO[List[(String, String)]] =
    ZIO.foreach(eShopNames)(name => testResource(s"composite/eshop/$name.graphql").map(name -> _))

  private def routes(graph: ComposedGraph, typeName: String, field: String): List[String] =
    graph.fieldRoutes.getOrElse(FieldCoordinate(typeName, field), Nil).map(_.name)

  // Internal lookups, the spec's scalars and @serializeAs stay out; @require arguments stay until it is supported.
  private val eShopClientSchema =
    """"The `Long` scalar type represents a signed 64-bit integer."
      |scalar Long @specifiedBy(url: "https://scalars.graphql.org/chillicream/long.html")
      |
      |type Product {
      |  inStock: Boolean!
      |  name: String!
      |  price: Long!
      |  reviews: [Review!]!
      |  shippingEstimate(weight: Long!, price: Long!): Long
      |  upc: String!
      |  weight: Long!
      |}
      |
      |type Query {
      |  me: User
      |  product(upc: ID!): Product
      |  review(id: ID!): Review
      |  topProducts(first: Int! = 5): [Product!]!
      |  user(id: ID!): User
      |  users: [User!]!
      |}
      |
      |type Review {
      |  author: User
      |  authorId: String!
      |  body: String!
      |  id: ID!
      |  product: Product
      |  productUpc: String!
      |}
      |
      |type User {
      |  birthday: Int
      |  id: ID!
      |  name: String
      |  reviews: [Review!]!
      |  username: String
      |}""".stripMargin

  private object FederationInventory extends GenericSchema[Any] {
    import auto._

    final case class ProductArgs(upc: String)
    @GQLKey("upc")
    final case class Product(upc: String, inStock: Boolean)
    final case class Query(restocked: List[Product])

    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen

    val api = graphQL(RootResolver(Query(List(Product("2", inStock = true))))) @@ federated(
      EntityResolver.from[ProductArgs](args => ZQuery.succeed(Some(Product(args.upc, Set("1", "2")(args.upc)))))
    )
  }

  private object Shelf extends GenericSchema[Any] {
    import auto._

    final case class Item(name: String)
    final case class Query(shelf: Item)

    val api = graphQL(RootResolver(Query(Item("shelf"))))
  }

  def spec = suite("CompositeSchemaSpec")(
    test("composes the eShop source schemas into the client schema") {
      eShopSchemas.map { schemas =>
        val graph = composeComposite(schemas: _*)
        assertTrue(graph.map(renderTypes) == Right(eShopClientSchema))
      }
    },
    test("resolves the eShop benchmark query across local Caliban subgraphs") {
      import CompositeEShop._
      for {
        runtime  <- Gateway
                      .compose(
                        Subgraph.graphql("accounts", Accounts.api),
                        Subgraph.graphql("inventory", Inventory.api),
                        Subgraph.graphql("products", Products.api),
                        Subgraph.graphql("reviews", Reviews.api)
                      )
                      .interpreter
        actual   <- runtime.execute(query)
        expected <- Monolith.api.interpreter.flatMap(_.execute(query)).orDie
      } yield assertTrue(actual.errors.isEmpty, expected.errors.isEmpty, actual.data == expected.data)
    },
    test("resolves entities across composite and Federation subgraphs") {
      for {
        runtime <- Gateway
                     .compose(
                       Subgraph.graphql("products", CompositeEShop.Products.api),
                       Subgraph.federation("inventory", FederationInventory.api)
                     )
                     .interpreter
        result  <- runtime.execute("{ topProducts(first: 3) { upc name inStock } restocked { upc name } }")
      } yield assertTrue(
        result.errors.isEmpty,
        result.data.toString ==
          """{"topProducts":[{"upc":"1","name":"Table","inStock":true},{"upc":"2","name":"Couch","inStock":true},{"upc":"3","name":"Glass","inStock":false}],"restocked":[{"upc":"2","name":"Couch"}]}"""
      )
    },
    suite("sharing")(
      test("requires a key or @shareable for a field several subgraphs resolve") {
        def product(root: String, directives: String) =
          s"type Query { $root: Product } type Product $directives { id: ID! name: String }"
        val unshared                                  = composeComposite("a" -> product("a", ""), "b" -> product("b", ""))
        val keyed                                     =
          composeComposite("a" -> product("a", "@key(fields: \"id\")"), "b" -> product("b", "@key(fields: \"id\")"))
        val shared                                    = composeComposite("a" -> product("a", "@shareable"), "b" -> product("b", "@shareable"))
        assertTrue(
          unshared.reports(Code.InvalidFieldSharing, Member("Product", "id"), "a", "b"),
          unshared.reports(Code.InvalidFieldSharing, Member("Product", "name"), "a", "b"),
          keyed.reportsOnly(Code.InvalidFieldSharing, Member("Product", "name"), "a", "b"),
          shared.isRight
        )
      },
      test("infers a key from a lookup's arguments") {
        val a = "type Query { a(id: ID!): Product @lookup } type Product { id: ID! }"
        val b = "type Query { b(id: ID!): Product @lookup } type Product { id: ID! name: String }"
        assertTrue(composeComposite("a" -> a, "b" -> b).isRight)
      },
      test("requires @shareable for root fields") {
        val a = "type Query { status: String }"
        assertTrue(
          composeComposite("a" -> a, "b" -> a).reports(Code.InvalidFieldSharing, Member("Query", "status"), "a", "b"),
          composeComposite(
            "a" -> a.replace("String", "String @shareable"),
            "b" -> a.replace("String", "String @shareable")
          ).isRight
        )
      },
      test("applies to local graphs and not to introspected ones") {
        for {
          local         <- compositionErrors(Subgraph.graphql("shelf", Shelf.api), Subgraph.graphql("store", Shelf.api))
          introspection <- introspectionResponse(Shelf.api)
          shelf         <- stub(introspection)
          store         <- stub(introspection)
          introspected  <-
            Gateway
              .compose(Subgraph.graphql("shelf", shelf.endpoint), Subgraph.graphql("store", store.endpoint))
              .interpreter
              .exit
        } yield assertTrue(
          local.reports(Code.InvalidFieldSharing, Member("Item", "name"), "shelf", "store"),
          introspected.isSuccess
        )
      }
    ),
    test("keeps @internal types and fields out of merging") {
      val a     = """type Query { productById(id: ID!): Product @lookup productBySku(sku: ID!): Product @lookup @internal }
                |type Product @key(fields: "id") { id: ID! sku: ID! }
                |type Secret @internal { value: String }""".stripMargin
      val b     = """type Query { productBySku(sku: Int!): Product }
                |type Product @key(fields: "id") { id: ID! }""".stripMargin
      val graph = composeComposite("a" -> a, "b" -> b)
      assertTrue(
        graph.map(routes(_, "Query", "productBySku")) == Right(List("b")),
        graph.exists(_.rootType.types.get("Secret").isEmpty),
        graph.exists(_.sources.find(_.name == "a").exists(_.entityLookups("Product").size == 2))
      )
    },
    test("hides the types only dropped directive definitions use") {
      val sdl     = """directive @d(opts: Opts, kind: Kind) on FIELD_DEFINITION
                  |input Opts { inner: Inner }
                  |input Inner { x: Int }
                  |enum Kind { A }
                  |type Query { a(kind: Kind): String @d }""".stripMargin
      val types   = composeComposite("a" -> sdl).map(_.rootType.types.keySet)
      val renamed = composeSdl(
        List("a" -> sdl),
        federation = false,
        Map("a"  -> List(SchemaTransformation.renameType("Opts", "Options")))
      ).map(_.rootType.types.keySet)
      assertTrue(
        types.exists(names => names("Kind") && !names("Opts") && !names("Inner")),
        renamed.exists(names => names("Kind") && !names("Options") && !names("Inner"))
      )
    },
    test("follows an override chain and rejects cycles and competing overrides") {
      def product(from: Option[String])                                         =
        s"type Product @key(fields: \"id\") { id: ID! price: Float ${from.fold("")(name => s"@override(from: \"$name\")")} }"
      def subgraphs(catalog: Option[String], payments: String, pricing: String) =
        composeComposite(
          "catalog"  -> s"type Query { products: [Product] } ${product(catalog)}",
          "payments" -> s"type Query { payments: String } ${product(Some(payments))}",
          "pricing"  -> s"type Query { pricing: String } ${product(Some(pricing))}"
        )
      val price                                                                 = Member("Product", "price")
      assertTrue(
        subgraphs(None, "catalog", "payments").map(routes(_, "Product", "price")) == Right(List("pricing")),
        subgraphs(Some("pricing"), "catalog", "payments")
          .reports(Code.OverrideSourceHasOverride, price, "catalog", "payments", "pricing"),
        subgraphs(None, "catalog", "catalog").reports(Code.OverrideSourceHasOverride, price, "payments", "pricing")
      )
    },
    test("serves @external fields through @provides and hides @inaccessible ones") {
      val reviews  = """type Query { reviews: [Review] }
                      |type Review { body: String author: User @provides(fields: "name") }
                      |type User @key(fields: "id") { id: ID! name: String @external }""".stripMargin
      val accounts =
        """type Query { user(id: ID!): User @lookup }
          |type User @key(fields: "id") { id: ID! name: String secret: String @inaccessible }""".stripMargin
      val graph    = composeComposite("accounts" -> accounts, "reviews" -> reviews)
      for {
        runtime <- Gateway
                     .compose(
                       Subgraph.graphql("accounts", unreachableEndpoint, accounts),
                       Subgraph.graphql("reviews", unreachableEndpoint, reviews)
                     )
                     .interpreter
        plan    <- runtime.explain("{ reviews { author { name } } }")
      } yield assertTrue(
        graph.map(routes(_, "User", "name")) == Right(List("accounts")),
        graph.exists(_.rootType.types.get("User").exists(!_.allFields.exists(_.name == "secret"))),
        plan.contains("reviews"),
        !plan.contains("accounts")
      )
    },
    test("fetches SDL with an HTTP GET and reads its directives") {
      for {
        sdl    <- testResource("composite/eshop/products.graphql")
        url    <- getEndpoint("products-sdl")(_ => ZIO.succeed(Response.text(sdl)))
        errors <- compositionErrors(
                    Subgraph.graphql("products", unreachableEndpoint, url),
                    Subgraph.graphql(
                      "shelf",
                      unreachableEndpoint,
                      "type Query { shelf: Product } type Product { name: String! }"
                    )
                  )
      } yield assertTrue(errors.reportsOnly(Code.InvalidFieldSharing, Member("Product", "name"), "products", "shelf"))
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.timeout(60.seconds)
}
