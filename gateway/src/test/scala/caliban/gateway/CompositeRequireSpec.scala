package caliban.gateway

import caliban.federation.EntityResolver
import caliban.federation.v2_6.{ federated, GQLKey }
import caliban.gateway.CompositionDiagnostic.Code
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.SchemaCoordinate.{ Argument, Member }
import caliban.schema.Annotations.GQLInterface
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, GraphQL, RootResolver }
import zio._
import zio.http._
import zio.query.ZQuery
import zio.test._

object CompositeRequireSpec extends ZIOSpecDefault {

  final case class ById(id: String)
  implicit val byIdArg: ArgBuilder[ById] = ArgBuilder.gen

  // The source of every requirement: products with fields of each shape a selection map reads.
  private object Products extends GenericSchema[Any] {
    import auto._

    final case class Dimension(size: Int, weight: Int)
    final case class Tag(name: String)
    final case class PriceArgs(withDiscount: Option[Boolean])
    final case class Product(
      id: String,
      weight: Int,
      nullableWeight: Option[Int],
      sku: Option[String],
      name: String,
      price: PriceArgs => Int,
      dimension: Dimension,
      tags: List[Tag]
    )
    final case class Query(products: List[Product], repeated: List[Product], productById: ById => Option[Product])

    implicit val priceArgs: ArgBuilder[PriceArgs] = ArgBuilder.gen

    private def product(id: String, weight: Int, nullable: Boolean, size: Int, price: Int, tags: List[String]) =
      Product(
        id,
        weight,
        Some(weight).filterNot(_ => nullable),
        Some(s"SKU-$id").filterNot(_ => nullable),
        s"name-$id",
        args => if (args.withDiscount.contains(true)) price * 8 / 10 else price,
        Dimension(size, weight),
        tags.map(Tag(_))
      )

    private val all = List(
      product("p1", 10, nullable = false, 2, 100, List("a", "b")),
      product("p2", 20, nullable = true, 3, 200, List("c"))
    )

    val api: GraphQL[Any] =
      graphQL(RootResolver(Query(all, all ::: all.take(1), args => all.find(_.id == args.id))))

    val sdl =
      """type Query { products: [Product!]! repeated: [Product!]! productById(id: ID!): Product @lookup }
        |type Product @key(fields: "id") {
        |  id: ID!
        |  weight: Int!
        |  nullableWeight: Int
        |  sku: String
        |  name: String!
        |  price(withDiscount: Boolean): Int!
        |  dimension: Dimension!
        |  tags: [Tag!]!
        |}
        |type Dimension { size: Int! weight: Int! }
        |type Tag { name: String! }""".stripMargin
  }

  // Fields filled by @require, each answering with the values it received.
  private object Shipping extends GenericSchema[Any] {
    import auto._

    final case class DimensionInput(productSize: Int, productWeight: Int)
    final case class WeightArgs(weight: Int)
    final case class SizeArgs(size: Int)
    final case class DeliveryArgs(zip: String, dimension: DimensionInput)
    final case class PriceArgs(price: Option[Int])
    final case class OptionalArgs(weight: Option[Int])
    final case class TagArgs(names: List[String])
    final case class CodeArgs(code: String)
    final case class Product(
      id: String,
      carrier: String,
      estimate: WeightArgs => Int,
      volume: SizeArgs => Int,
      delivery: DeliveryArgs => String,
      discounted: PriceArgs => Option[Int],
      regular: PriceArgs => Option[Int],
      optional: OptionalArgs => String,
      strict: WeightArgs => Option[String],
      both: DimensionInput => Option[String],
      tagged: TagArgs => String,
      code: CodeArgs => String
    )
    final case class ByIds(ids: List[String])
    final case class Query(
      productById: ById => Option[Product],
      productsByIds: ByIds => List[Product],
      cheapest: Product
    )

    implicit val dimensionInput: ArgBuilder[DimensionInput]   = ArgBuilder.gen
    implicit val weightArgs: ArgBuilder[WeightArgs]           = ArgBuilder.gen
    implicit val sizeArgs: ArgBuilder[SizeArgs]               = ArgBuilder.gen
    implicit val deliveryArgs: ArgBuilder[DeliveryArgs]       = ArgBuilder.gen
    implicit val priceArgs: ArgBuilder[PriceArgs]             = ArgBuilder.gen
    implicit val optionalArgs: ArgBuilder[OptionalArgs]       = ArgBuilder.gen
    implicit val tagArgs: ArgBuilder[TagArgs]                 = ArgBuilder.gen
    implicit val codeArgs: ArgBuilder[CodeArgs]               = ArgBuilder.gen
    implicit val byIds: ArgBuilder[ByIds]                     = ArgBuilder.gen
    implicit val dimensionSchema: Schema[Any, DimensionInput] = Schema.gen

    private def product(id: String) =
      Product(
        id,
        s"carrier-$id",
        args => args.weight * 2,
        args => args.size * 100,
        args => s"${args.zip}:${args.dimension.productSize}x${args.dimension.productWeight}",
        _.price,
        _.price,
        _.weight.fold("none")(_.toString),
        args => Some(args.weight.toString),
        args => Some(s"${args.productSize}x${args.productWeight}"),
        _.names.mkString(","),
        _.code
      )

    val api: GraphQL[Any] =
      graphQL(RootResolver(Query(args => Some(product(args.id)), _.ids.map(product), product("p2"))))

    private def sdlWith(query: String): String =
      s"""type Query { $query }
         |type Product @key(fields: "id") {
         |  id: ID!
         |  carrier: String!
         |  estimate(weight: Int! @require(field: "weight")): Int!
         |  volume(size: Int! @require(field: "dimension.size")): Int!
         |  delivery(
         |    zip: String!
         |    dimension: DimensionInput! @require(field: "{ productSize: dimension.size, productWeight: dimension.weight }")
         |  ): String!
         |  discounted(price: Int @require(field: "price(withDiscount: true)")): Int
         |  regular(price: Int @require(field: "price(withDiscount: false)")): Int
         |  optional(weight: Int @require(field: "nullableWeight")): String!
         |  strict(weight: Int! @require(field: "nullableWeight")): String
         |  both(productSize: Int! @require(field: "dimension.size"), productWeight: Int! @require(field: "nullableWeight")): String
         |  tagged(names: [String!]! @require(field: "tags[name]")): String!
         |  code(code: String! @require(field: "sku | name")): String!
         |}
         |input DimensionInput { productSize: Int! productWeight: Int! }""".stripMargin

    val sdl = sdlWith("productById(id: ID!): Product @lookup @internal cheapest: Product!")

    // The same fields, resolved only through a lookup declared in Scala that takes a list of keys.
    val listSdl = sdlWith("productsByIds(ids: [ID!]!): [Product!]! @internal")
  }

  private object Accounts extends GenericSchema[Any] {
    import auto._

    @GQLInterface
    sealed trait Account
    object Account {
      final case class User(id: String, preferredLocale: Option[String]) extends Account
      final case class Organization(id: String, preferredLocale: Option[String], billingLocale: Option[String])
          extends Account
    }
    final case class Query(accounts: List[Account])

    val api: GraphQL[Any] =
      graphQL(
        RootResolver(Query(List(Account.User("u1", Some("fr")), Account.Organization("o1", Some("en"), Some("de")))))
      )

    val sdl =
      """type Query { accounts: [Account!]! }
        |interface Account { id: ID! preferredLocale: String }
        |type User implements Account @key(fields: "id") { id: ID! preferredLocale: String }
        |type Organization implements Account @key(fields: "id") {
        |  id: ID!
        |  preferredLocale: String
        |  billingLocale: String
        |}""".stripMargin
  }

  private object Greetings extends GenericSchema[Any] {
    import auto._

    final case class LocaleArgs(locale: Option[String])
    @GQLInterface
    sealed trait Account
    object Account {
      final case class User(id: String, displayName: LocaleArgs => String)         extends Account
      final case class Organization(id: String, displayName: LocaleArgs => String) extends Account
    }
    final case class Query(accountById: ById => Option[Account])

    implicit val localeArgs: ArgBuilder[LocaleArgs] = ArgBuilder.gen

    private def greet(id: String): LocaleArgs => String = args => s"$id@${args.locale.getOrElse("none")}"

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(args =>
          Some(
            if (args.id.startsWith("u")) Account.User(args.id, greet(args.id))
            else Account.Organization(args.id, greet(args.id))
          )
        )
      )
    )

    val sdl =
      """type Query { accountById(id: ID!): Account @lookup @internal }
        |interface Account { id: ID! displayName(locale: String @require(field: "preferredLocale")): String! }
        |type User implements Account @key(fields: "id") {
        |  id: ID!
        |  displayName(locale: String @require(field: "preferredLocale")): String!
        |}
        |type Organization implements Account @key(fields: "id") {
        |  id: ID!
        |  displayName(locale: String @require(field: "billingLocale")): String!
        |}""".stripMargin
  }

  private object FederationProducts extends GenericSchema[Any] {
    import auto._

    final case class ProductArgs(id: String)
    final case class Product(id: String, weight: Int)
    final case class Query(products: List[Product])

    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen
    // A hand-written instance, since deriving one under @GQLKey crashes Scala 2.13 in this file.
    implicit val productSchema: Schema[Any, Product]         =
      obj("Product", directives = List(GQLKey("id").directive))(implicit attributes =>
        List(field("id")(_.id), field("weight")(_.weight))
      )

    private def product(id: String) = Product(id, if (id == "p1") 10 else 20)

    val api = graphQL(RootResolver(Query(List(product("p1"), product("p2"))))) @@ federated(
      EntityResolver.from[ProductArgs](args => ZQuery.succeed(Some(product(args.id))))
    )
  }

  private final case class ShippingGateway(runtime: GatewayInterpreter[Any], shipping: Stub)

  private def shippingGateway: ZIO[Server with Ref[Int] with Scope, Nothing, ShippingGateway] =
    shippingGatewayWith(Subgraph.graphql("shipping", _, Shipping.sdl))

  private def shippingGatewayWith(
    shippingSubgraph: URL => Subgraph[Any],
    productsSdl: String = Products.sdl
  ): ZIO[Server with Ref[Int] with Scope, Nothing, ShippingGateway] =
    for {
      products <- served(Products.api)
      shipping <- served(Shipping.api)
      runtime  <-
        Gateway
          .compose(Subgraph.graphql("products", products.endpoint, productsSdl), shippingSubgraph(shipping.endpoint))
          .interpreter
          .orDie
    } yield ShippingGateway(runtime, shipping)

  private def product(fields: String) =
    s"""type Query { product: Product } type Product @key(fields: "id") { id: ID! $fields }"""

  def spec = suite("CompositeRequireSpec")(
    suite("composition")(
      test("hides @require arguments and the input types only they use") {
        val graph              = composeComposite("products" -> Products.sdl, "shipping" -> Shipping.sdl)
        val fields             = graph.map(_.rootType.types.get("Product").toList.flatMap(_.allFields))
        def args(name: String) = fields.map(_.find(_.name == name).toList.flatMap(_.allArgs.map(_.name)))
        assertTrue(
          args("delivery") == Right(List("zip")),
          args("estimate") == Right(Nil),
          graph.exists(!_.rootType.types.contains("DimensionInput"))
        )
      },
      test("keeps an input type that a client argument also uses") {
        val shipping = Shipping.sdl.replace("carrier: String!", "carrier(dimension: DimensionInput): String!")
        val graph    = composeComposite("products" -> Products.sdl, "shipping" -> shipping)
        assertTrue(graph.exists(_.rootType.types.contains("DimensionInput")))
      },
      test("composes the spec's examples") {
        val dimensions                                     = """type Query { products: [Product] }
                           |type Product @key(fields: "id") { id: ID! dimension: ProductDimension! }
                           |type ProductDimension @shareable { size: Int! weight: Int! }""".stripMargin
        def delivery(argument: String, extra: String = "") =
          s"""type Query { deliveries: [DeliveryEstimates] }
             |type Product @key(fields: "id") { id: ID! delivery(zip: String! $argument): DeliveryEstimates }
             |type DeliveryEstimates { min: Int }
             |$extra""".stripMargin
        val paths                                          = delivery(
          """size: Int! @require(field: "dimension.size") weight: Int! @require(field: "dimension.weight")"""
        )
        val inputs                                         = delivery(
          """dimension: ProductDimensionInput! @require(field: "{ size: dimension.size, weight: dimension.weight }")""",
          "input ProductDimensionInput { size: Int! weight: Int! }"
        )
        val renamed                                        = delivery(
          """dimension: ProductDimensionInput!
            |  @require(field: "{ productSize: dimension.size, productWeight: dimension.weight }")""".stripMargin,
          "input ProductDimensionInput { productSize: Int! productWeight: Int! }"
        )
        val weights                                        = """type Query { products: [Product] }
                        |type Product @key(fields: "id") { id: ID! weight(unit: WeightUnit!): Float }
                        |enum WeightUnit { METRIC IMPERIAL }""".stripMargin
        val shipping                                       = """type Query { costs: [Currency] }
                         |scalar Currency
                         |type Product @key(fields: "id") {
                         |  id: ID!
                         |  shippingCost(weight: Float @require(field: "weight(unit: IMPERIAL)")): Currency
                         |}""".stripMargin
        assertTrue(
          composeComposite("dimensions" -> dimensions, "delivery" -> paths).isRight,
          composeComposite("dimensions" -> dimensions, "delivery" -> inputs).isRight,
          composeComposite("dimensions" -> dimensions, "delivery" -> renamed).isRight,
          composeComposite("weights" -> weights, "shipping" -> shipping).isRight,
          composeComposite("accounts" -> Accounts.sdl, "greetings" -> Greetings.sdl).isRight
        )
      }
    ),
    suite("rejections")(
      test("reports the spec's source schema rules") {
        val profile                                    = Argument("User", "profile", "name")
        def user(argument: String, extra: String = "") =
          s"""type Query { users: [User] }
             |type User @key(fields: "id") { id: ID! profile($argument): Profile $extra }
             |type Profile { id: ID! name: String }""".stripMargin
        val lookup                                     = """type Query {
                       |  productById(id: ID!, locale: String @require(field: "defaultLocale")): Product @lookup
                       |}
                       |type Product @key(fields: "id") { id: ID! }""".stripMargin
        assertTrue(
          composeComposite("a" -> user("""name: String! @require(field: "{ name ")"""))
            .reports(Code.RequireInvalidSyntax, profile, "a"),
          composeComposite("a" -> user("name: String! @require(field: 123)"))
            .reports(Code.RequireInvalidFieldType, profile, "a"),
          composeComposite("a" -> lookup)
            .reports(Code.RequireInvalidUsage, Argument("Query", "productById", "locale"), "a"),
          composeComposite(
            "a" -> "type Query { books: [Book] } type Book { id: ID! title: String subtitle: String }",
            "b" -> """type Query { b: Book }
                     |type Book { id: ID! title(subtitle: String @require(field: "subtitle")): String @external }""".stripMargin
          ).reports(Code.ExternalRequireCollision, Member("Book", "title"), "b")
        )
      },
      test("requires @require consistently on interface fields and their implementations") {
        val inconsistent = """type Query { accounts: [Account] }
                             |interface Account { id: ID! displayName(locale: String): String }
                             |type User implements Account @key(fields: "id") {
                             |  id: ID!
                             |  displayName(locale: String @require(field: "preferredLocale")): String
                             |}""".stripMargin
        val locales      =
          """type Query { users: [User] } type User @key(fields: "id") { id: ID! preferredLocale: String }"""
        assertTrue(
          composeComposite("a" -> inconsistent, "b" -> locales)
            .reports(Code.RequireInconsistentOnImplementation, Argument("User", "displayName", "locale"), "a")
        )
      },
      test("validates selection maps against the fields of other subgraphs") {
        val pages                = Argument("Book", "pages", "pageSize")
        val cost                 = Argument("Product", "shippingCost", "size")
        def book(fields: String) = s"type Query { books: [Book] } type Book { id: ID! $fields }"
        val dimension            = """type Query { b: Product }
                          |type Product @key(fields: "id") { id: ID! dimension(unit: Unit!): Int! }
                          |enum Unit { METRIC IMPERIAL }""".stripMargin
        assertTrue(
          composeComposite("a" -> book("""pages(pageSize: Int @require(field: "unknownField")): Int"""))
            .reports(Code.RequireInvalidFields, pages, "a"),
          composeComposite("a" -> book("""size: Int pages(pageSize: Int @require(field: "size")): Int"""))
            .reports(Code.RequireInvalidFields, pages, "a"),
          composeComposite(
            "a" -> book("""size: Int @shareable pages(pageSize: Int @require(field: "size")): Int"""),
            "b" -> "type Query { b: Book } type Book { id: ID! size: Int @shareable }"
          ).reports(Code.RequireInvalidFields, pages, "a"),
          composeComposite(
            "a" -> product("""shippingCost(size: Int! @require(field: "dimension(unit: METRIC)")): Float"""),
            "b" -> dimension
          ).isRight,
          composeComposite(
            "a" -> product("""shippingCost(size: Int! @require(field: "dimension(scale: METRIC)")): Float"""),
            "b" -> dimension
          ).reports(Code.RequireInvalidFields, cost, "a")
        )
      },
      test("rejects an argument filled by @require that another subgraph requires from clients") {
        def collection(root: String, argument: String) =
          s"type Query { $root: Collection } type Collection { books($argument): [String] @shareable }"
        val authors                                    = "type Query { c: Collection } type Collection { author: String! }"
        val required                                   = collection("a", """author: String! @require(field: "author")""")
        assertTrue(
          composeComposite("a" -> required, "b" -> collection("b", "author: String"), "c" -> authors).isRight,
          composeComposite("a" -> required, "b" -> collection("b", "author: String!"), "c" -> authors)
            .reports(Code.FieldWithMissingRequiredArgument, Argument("Collection", "books", "author"), "a")
        )
      },
      test("reports both rules when @fromContext and @require supply the same argument") {
        val author   = Argument("Collection", "books", "author")
        val contexts = s"""${contextSchemaPreamble("v2.9", "@key", "@shareable", "@context", "@fromContext")}
                          |type Query { shelf: Shelf }
                          |type Shelf @context(name: "shelf") { author: String! collection: Collection }
                          |type Collection @key(fields: "id") {
                          |  id: ID!
                          |  books(author: String @fromContext(field: "$$shelf { author }")): [String] @shareable
                          |}""".stripMargin
        val required =
          """type Query { b: Collection }
            |type Collection { books(author: String! @require(field: "author")): [String] @shareable }""".stripMargin
        val clients  =
          "type Query { c: Collection } type Collection { author: String! books(author: String!): [String] @shareable }"
        compositionErrors(
          Subgraph.federation("a", unreachableEndpoint, contexts),
          Subgraph.graphql("b", unreachableEndpoint, required),
          Subgraph.graphql("c", unreachableEndpoint, clients)
        ).map(errors =>
          assertTrue(
            errors.reports(Code.ContextualArgumentNotContextualInAllSubgraphs, author, "c"),
            errors.reports(Code.FieldWithMissingRequiredArgument, author, "b")
          )
        )
      }
    ),
    suite("resolution")(
      test("fills leaf, path, object, list, choice and constant-argument requirements per entity") {
        for {
          gateway <- shippingGateway
          result  <-
            gateway.runtime.execute(
              """{ products { id estimate volume delivery(zip: "75001") tagged code discounted regular price } }"""
            )
          sent    <- queries(gateway.shipping)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString ==
            """{"products":[{"id":"p1","estimate":20,"volume":200,"delivery":"75001:2x10","tagged":"a,b","code":"SKU-p1","discounted":80,"regular":100,"price":100},{"id":"p2","estimate":40,"volume":300,"delivery":"75001:3x20","tagged":"c","code":"name-p2","discounted":160,"regular":200,"price":200}]}""",
          sent.size == 1,
          sent.exists(_.contains("""dimension:{productSize:2,productWeight:10}"""))
        )
      },
      test("sends null for a nullable argument, and nulls only the field whose non-null argument has none") {
        for {
          gateway <- shippingGateway
          result  <- gateway.runtime.execute("{ products { id carrier optional strict both } }")
        } yield assertTrue(
          result.data.toString ==
            """{"products":[{"id":"p1","carrier":"carrier-p1","optional":"10","strict":"10","both":"2x10"},{"id":"p2","carrier":"carrier-p2","optional":"none","strict":null,"both":null}]}""",
          result.errors.map(_.toResponseValue.toString) == List(
            """{"message":"The @require value of 'Product.strict(weight:)' is missing or invalid.","path":["products",1,"strict"]}""",
            """{"message":"The @require value of 'Product.both(productWeight:)' is missing or invalid.","path":["products",1,"both"]}"""
          )
        )
      },
      test("reads a requirement from an @inaccessible field") {
        val hidden = Products.sdl.replaceFirst("weight: Int!", "weight: Int! @inaccessible")
        for {
          gateway <- shippingGatewayWith(Subgraph.graphql("shipping", _, Shipping.sdl), hidden)
          result  <- gateway.runtime.execute("{ products { id estimate } }")
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == """{"products":[{"id":"p1","estimate":20},{"id":"p2","estimate":40}]}"""
        )
      },
      test("skips the call for an entity whose only fields have no argument value") {
        for {
          gateway <- shippingGateway
          result  <- gateway.runtime.execute("{ products { strict } }")
          sent    <- queries(gateway.shipping)
        } yield assertTrue(
          result.data.toString == """{"products":[{"strict":"10"},{"strict":null}]}""",
          result.errors.size == 1,
          sent.size == 1,
          !sent.exists(_.contains("lookup_1"))
        )
      },
      test("fetches entities with equal keys and requirement values once") {
        for {
          gateway <- shippingGateway
          result  <- gateway.runtime.execute("{ repeated { estimate } }")
          sent    <- queries(gateway.shipping)
        } yield assertTrue(
          result.data.toString == """{"repeated":[{"estimate":20},{"estimate":40},{"estimate":20}]}""",
          sent.exists(query => query.contains("lookup_1") && !query.contains("lookup_2"))
        )
      },
      test("returns to the requiring subgraph after reading what it requires") {
        for {
          gateway <- shippingGateway
          result  <- gateway.runtime.execute("{ cheapest { id estimate } }")
          sent    <- queries(gateway.shipping)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == """{"cheapest":{"id":"p2","estimate":40}}""",
          sent.exists(_.contains("estimate(weight:20)"))
        )
      },
      test("calls a list lookup once per distinct set of argument values") {
        val lookup = Lookup.list("Product", "productsByIds", "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id")))
        for {
          gateway <- shippingGatewayWith(Subgraph.graphql("shipping", _, Shipping.listSdl).withLookup(lookup))
          result  <- gateway.runtime.execute("{ repeated { id estimate carrier } }")
          sent    <- queries(gateway.shipping)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString ==
            """{"repeated":[{"id":"p1","estimate":20,"carrier":"carrier-p1"},{"id":"p2","estimate":40,"carrier":"carrier-p2"},{"id":"p1","estimate":20,"carrier":"carrier-p1"}]}""",
          sent.size == 2,
          sent.exists(_.contains("estimate(weight:10)")),
          sent.exists(_.contains("estimate(weight:20)"))
        )
      },
      test("uses each implementation's selection map for an interface field") {
        for {
          accounts  <- served(Accounts.api)
          greetings <- served(Greetings.api)
          runtime   <- Gateway
                         .compose(
                           Subgraph.graphql("accounts", accounts.endpoint, Accounts.sdl),
                           Subgraph.graphql("greetings", greetings.endpoint, Greetings.sdl)
                         )
                         .interpreter
          result    <- runtime.execute("{ accounts { id displayName } }")
          fragment  <- runtime.execute("{ accounts { id ... on Organization { displayName } } }")
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == """{"accounts":[{"id":"u1","displayName":"u1@fr"},{"id":"o1","displayName":"o1@de"}]}""",
          fragment.errors.isEmpty,
          fragment.data.toString == """{"accounts":[{"id":"u1"},{"id":"o1","displayName":"o1@de"}]}"""
        )
      },
      test("reads requirements from a Federation subgraph") {
        for {
          shipping <- served(Shipping.api)
          runtime  <- Gateway
                        .compose(
                          Subgraph.federation("products", FederationProducts.api),
                          Subgraph.graphql(
                            "shipping",
                            shipping.endpoint,
                            """type Query { productById(id: ID!): Product @lookup @internal }
                             |type Product @key(fields: "id") {
                             |  id: String!
                             |  estimate(weight: Int! @require(field: "weight")): Int!
                             |}""".stripMargin
                          )
                        )
                        .interpreter
          result   <- runtime.execute("{ products { id estimate } }")
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == """{"products":[{"id":"p1","estimate":20},{"id":"p2","estimate":40}]}"""
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
}
