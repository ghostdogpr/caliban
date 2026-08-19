package caliban.gateway

import caliban.ResponseValue.ListValue
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, CalibanError, PathValue, RootResolver }
import zio._
import zio.test._

object LookupSpec extends ZIOSpecDefault {

  private object LocalReviews extends GenericSchema[Any] {
    import auto._

    final case class ProductRefInput(productId: String)
    final case class ProductRefs(refs: List[ProductRefInput])
    final case class Review(body: String)
    final case class Product(id: String, reviews: List[Review])
    final case class Query(productsByRefs: ProductRefs => UIO[List[Product]])

    implicit val productRefBuilder: ArgBuilder[ProductRefInput] = ArgBuilder.gen
    implicit val productRefsBuilder: ArgBuilder[ProductRefs]    = ArgBuilder.gen
    implicit val querySchema: Schema[Any, Query]                = gen

    val api = graphQL(
      RootResolver(
        Query(args =>
          ZIO.succeed(args.refs.reverse.map(ref => Product(ref.productId, List(Review(s"${ref.productId} review")))))
        )
      )
    )
  }

  private val productsSchema =
    """
      |type Query {
      |  products: [Product]
      |  status: String!
      |}
      |
      |type Product {
      |  id: ID!
      |  region: String!
      |  name: String!
      |}
      |""".stripMargin

  private val reviewsSchema =
    """
      |input ProductRefInput {
      |  productId: ID!
      |  regionCode: String!
      |}
      |
      |type Query {
      |  productsByRefs(refs: [ProductRefInput!]!): [Product!]!
      |  productByRef(ref: ProductRefInput!): Product
      |}
      |
      |type Product {
      |  id: ID!
      |  region: String!
      |  reviews: [Review!]!
      |}
      |
      |type Review {
      |  body: String!
      |}
      |""".stripMargin

  private val keyFields = List("id", "region")

  private val refArgument = Lookup.Argument.obj(
    "productId"  -> Lookup.Argument.key("id"),
    "regionCode" -> Lookup.Argument.key("region")
  )

  private val keyedLookup = Lookup.list(
    typeName = "Product",
    keyFields = keyFields,
    field = "productsByRefs",
    arguments = Map("refs" -> Lookup.Argument.batch(refArgument)),
    correlation = Lookup.Correlation.byKey(Map("id" -> "id", "region" -> "region"))
  )

  private val orderedLookup = Lookup.list(
    typeName = "Product",
    keyFields = keyFields,
    field = "productsByRefs",
    arguments = Map("refs" -> Lookup.Argument.batch(refArgument)),
    correlation = Lookup.Correlation.ordered
  )

  private val singleLookup = Lookup.single(
    typeName = "Product",
    keyFields = keyFields,
    field = "productByRef",
    arguments = Map("ref" -> refArgument)
  )

  private val productsResponse =
    """{"data":{"status":"available","products":[{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_key_2":"us"},{"name":"Chair","_caliban_gateway_key":"p2","_caliban_gateway_key_2":"eu"}]}}"""

  private val enumProductsSchema =
    """
      |enum Region {
      |  US
      |  EU
      |}
      |
      |type Query {
      |  products: [Product]
      |}
      |
      |type Product {
      |  id: ID!
      |  region: Region!
      |  name: String!
      |}
      |""".stripMargin

  private val enumReviewsSchema =
    """
      |enum Region {
      |  US
      |  EU
      |}
      |
      |input ProductRefInput {
      |  productId: ID!
      |  regionCode: Region!
      |}
      |
      |type Query {
      |  productsByRefs(refs: [ProductRefInput!]!): [Product!]!
      |  productByRef(ref: ProductRefInput!): Product
      |}
      |
      |type Product {
      |  id: ID!
      |  region: Region!
      |  reviews: [Review!]!
      |}
      |
      |type Review {
      |  body: String!
      |}
      |""".stripMargin

  def spec = suite("LookupSpec")(
    test("batches compound arguments and correlates ordinary lookup results by key") {
      val reviewsResponse =
        """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p2","_caliban_gateway_lookup_key_2":"eu","reviews":[{"body":"Chair review"}]},{"_caliban_gateway_lookup_key":"p1","_caliban_gateway_lookup_key_2":"us","reviews":[{"body":"Table review"}]}]}}"""

      for {
        products <- stub(productsResponse)
        reviews  <- stub(reviewsResponse)
        gateway  <- Gateway
                      .compose(
                        Subgraph.graphql("products", products.endpoint, productsSchema),
                        Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema).withLookup(keyedLookup)
                      )
                      .build
        response <- gateway.execute("{ status products { name reviews { body } } }")
        requests <- reviews.requests.get
        valid    <- ZIO.foreach(requests)(validateRequest(reviewsSchema, _).exit)
        values    = field(response.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "status").contains(StringValue("available")),
        values.flatMap(field(_, "name")) == List(StringValue("Table"), StringValue("Chair")),
        values.flatMap(reviewBody) ==
          List(StringValue("Table review"), StringValue("Chair review")),
        requests.size == 1,
        valid.forall(_.isSuccess),
        requests.headOption
          .flatMap(_.query)
          .exists(query =>
            query.contains("productsByRefs") &&
              query.contains("{productId:\"p1\",regionCode:\"us\"}") &&
              query.contains("{productId:\"p2\",regionCode:\"eu\"}")
          )
      )
    },
    test("skips a lookup when an entity key component is null") {
      val nullKeyResponse =
        """{"data":{"status":"available","products":[{"name":"Unknown","_caliban_gateway_key":"p1","_caliban_gateway_key_2":null}]}}"""

      for {
        products <- stub(nullKeyResponse)
        reviews  <- stub("""{"data":{"_caliban_gateway_lookup":[]}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph.graphql("products", products.endpoint, productsSchema),
                        Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema).withLookup(keyedLookup)
                      )
                      .build
        response <- gateway.execute("{ products { name reviews { body } } }")
        sent     <- reviews.requests.get
      } yield assertTrue(
        sent.isEmpty,
        response.errors.exists(_.msg.contains("Entity key 'Product(id, region)' was missing")),
        field(response.data, "products").contains(ListValue(List(NullValue)))
      )
    },
    test("executes an ordinary lookup against a local Caliban subgraph") {
      val localProductsSchema   =
        "type Query { products: [Product]! } type Product { id: String! name: String! }"
      val localProductsResponse =
        """{"data":{"products":[{"name":"Table","_caliban_gateway_key":"p1"},{"name":"Chair","_caliban_gateway_key":"p2"}]}}"""
      val localLookup           = Lookup.list(
        typeName = "Product",
        keyFields = List("id"),
        field = "productsByRefs",
        arguments = Map(
          "refs" -> Lookup.Argument.batch(
            Lookup.Argument.obj("productId" -> Lookup.Argument.key("id"))
          )
        ),
        correlation = Lookup.Correlation.byKey(Map("id" -> "id"))
      )

      for {
        products <- stub(localProductsResponse)
        gateway  <- Gateway
                      .compose(
                        Subgraph.graphql("products", products.endpoint, localProductsSchema),
                        Subgraph.local("reviews", LocalReviews.api).withLookup(localLookup)
                      )
                      .build
        response <- gateway.execute("{ products { name reviews { body } } }")
        values    = field(response.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
      } yield assertTrue(
        response.errors.isEmpty,
        values.flatMap(field(_, "name")) == List(StringValue("Table"), StringValue("Chair")),
        values.flatMap(reviewBody) == List(StringValue("p1 review"), StringValue("p2 review"))
      )
    },
    test("correlates list lookups by order and single lookups by generated alias") {
      val orderedResponse =
        """{"data":{"_caliban_gateway_lookup":[{"reviews":[{"body":"Table review"}]},{"reviews":[{"body":"Chair review"}]}]}}"""
      val singleResponse  =
        """{"data":{"_caliban_gateway_lookup_0":{"reviews":[{"body":"Table review"}]},"_caliban_gateway_lookup_1":{"reviews":[{"body":"Chair review"}]}}}"""

      for {
        productsA <- stub(productsResponse)
        reviewsA  <- stub(orderedResponse)
        ordered   <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsA.endpoint, productsSchema),
                         Subgraph.graphql("reviews", reviewsA.endpoint, reviewsSchema).withLookup(orderedLookup)
                       )
                       .build
        responseA <- ordered.execute("{ products { name reviews { body } } }")
        productsB <- stub(productsResponse)
        reviewsB  <- stub(singleResponse)
        single    <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsB.endpoint, productsSchema),
                         Subgraph.graphql("reviews", reviewsB.endpoint, reviewsSchema).withLookup(singleLookup)
                       )
                       .build
        responseB <- single.execute("{ products { name reviews { body } } }")
        sentB     <- reviewsB.requests.get
        ordered    = field(responseA.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
        singles    = field(responseB.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
      } yield assertTrue(
        responseA.errors.isEmpty,
        responseB.errors.isEmpty,
        ordered.flatMap(reviewBody) ==
          List(StringValue("Table review"), StringValue("Chair review")),
        singles.flatMap(reviewBody) ==
          List(StringValue("Table review"), StringValue("Chair review")),
        sentB.size == 1,
        sentB.headOption
          .flatMap(_.query)
          .exists(query =>
            query.contains("_caliban_gateway_lookup_0:productByRef") &&
              query.contains("_caliban_gateway_lookup_1:productByRef")
          )
      )
    },
    test("renders enum keys in remote compound inputs for list and single lookups") {
      val enumProductsResponse =
        """{"data":{"products":[{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_key_2":"US"},{"name":"Chair","_caliban_gateway_key":"p2","_caliban_gateway_key_2":"EU"}]}}"""
      val listResponse         =
        """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p2","_caliban_gateway_lookup_key_2":"EU","reviews":[{"body":"Chair review"}]},{"_caliban_gateway_lookup_key":"p1","_caliban_gateway_lookup_key_2":"US","reviews":[{"body":"Table review"}]}]}}"""
      val singleResponse       =
        """{"data":{"_caliban_gateway_lookup_0":{"reviews":[{"body":"Table review"}]},"_caliban_gateway_lookup_1":{"reviews":[{"body":"Chair review"}]}}}"""

      for {
        productsA <- stub(enumProductsResponse)
        reviewsA  <- stub(listResponse)
        list      <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsA.endpoint, enumProductsSchema),
                         Subgraph.graphql("reviews", reviewsA.endpoint, enumReviewsSchema).withLookup(keyedLookup)
                       )
                       .build
        responseA <- list.execute("{ products { name reviews { body } } }")
        sentA     <- reviewsA.requests.get
        validA    <- ZIO.foreach(sentA)(validateRequest(enumReviewsSchema, _).exit)
        productsB <- stub(enumProductsResponse)
        reviewsB  <- stub(singleResponse)
        single    <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsB.endpoint, enumProductsSchema),
                         Subgraph.graphql("reviews", reviewsB.endpoint, enumReviewsSchema).withLookup(singleLookup)
                       )
                       .build
        responseB <- single.execute("{ products { name reviews { body } } }")
        sentB     <- reviewsB.requests.get
        validB    <- ZIO.foreach(sentB)(validateRequest(enumReviewsSchema, _).exit)
      } yield assertTrue(
        responseA.errors.isEmpty,
        responseB.errors.isEmpty,
        validA.forall(_.isSuccess),
        validB.forall(_.isSuccess),
        sentA.headOption.flatMap(_.query).exists(_.contains("regionCode:US")),
        sentB.headOption.flatMap(_.query).exists(_.contains("regionCode:EU"))
      )
    },
    test("allows keyed omissions but rejects short ordered lookup results") {
      val shortResponse =
        """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p1","_caliban_gateway_lookup_key_2":"us","reviews":[{"body":"Table review"}]}]}}"""

      for {
        productsA <- stub(productsResponse)
        reviewsA  <- stub(shortResponse)
        keyed     <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsA.endpoint, productsSchema),
                         Subgraph.graphql("reviews", reviewsA.endpoint, reviewsSchema).withLookup(keyedLookup)
                       )
                       .build
        responseA <- keyed.execute("{ products { name reviews { body } } }")
        productsB <- stub(productsResponse)
        reviewsB  <- stub(shortResponse)
        ordered   <- Gateway
                       .compose(
                         Subgraph.graphql("products", productsB.endpoint, productsSchema),
                         Subgraph.graphql("reviews", reviewsB.endpoint, reviewsSchema).withLookup(orderedLookup)
                       )
                       .build
        responseB <- ordered.execute("{ products { name reviews { body } } }")
        valuesA    = field(responseA.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
      } yield assertTrue(
        valuesA.size == 2,
        valuesA.lift(1).contains(NullValue),
        responseA.errors.map(_.msg) == List("Cannot return null for non-nullable field Product.reviews."),
        !responseA.errors.exists(_.msg.contains("omitted a result")),
        responseB.errors.exists(_.msg.contains("omitted a result"))
      )
    },
    test("rejects an extra result from an ordered lookup") {
      val reviewsResponse =
        """{"data":{"_caliban_gateway_lookup":[{"reviews":[{"body":"Table review"}]},{"reviews":[{"body":"Chair review"}]},{"reviews":[{"body":"Unexpected review"}]}]}}"""

      for {
        products <- stub(productsResponse)
        reviews  <- stub(reviewsResponse)
        gateway  <- Gateway
                      .compose(
                        Subgraph.graphql("products", products.endpoint, productsSchema),
                        Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema).withLookup(orderedLookup)
                      )
                      .build
        response <- gateway.execute("{ products { name reviews { body } } }")
        values    = field(response.data, "products").collect { case ListValue(values) => values }.getOrElse(Nil)
      } yield assertTrue(
        values.flatMap(reviewBody) == List(StringValue("Table review"), StringValue("Chair review")),
        response.errors.map(_.msg) ==
          List("Entity lookup response contained an unexpected result for 'Product(id, region)'."),
        response.errors.collect { case error: CalibanError.ExecutionError => error.path } ==
          List(List(PathValue.Key("products")))
      )
    },
    test("relocates ordinary lookup failures without losing independent data") {
      val reviewsResponse =
        """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p1","_caliban_gateway_lookup_key_2":"us","reviews":null}]},"errors":[{"message":"reviews unavailable","path":["_caliban_gateway_lookup",0,"reviews"],"extensions":{"code":"REVIEWS_DOWN"}}]}"""

      for {
        products <-
          stub(
            """{"data":{"status":"available","products":[{"name":"Table","_caliban_gateway_key":"p1","_caliban_gateway_key_2":"us"}]}}"""
          )
        reviews  <- stub(reviewsResponse)
        gateway  <- Gateway
                      .compose(
                        Subgraph.graphql("products", products.endpoint, productsSchema),
                        Subgraph.graphql("reviews", reviews.endpoint, reviewsSchema).withLookup(keyedLookup)
                      )
                      .build
        response <- gateway.execute("{ status products { name reviews { body } } }")
        errors    = response.errors.collect { case error: CalibanError.ExecutionError => error }
      } yield assertTrue(
        field(response.data, "status").contains(StringValue("available")),
        field(response.data, "products").contains(ListValue(List(NullValue))),
        errors.map(_.msg) == List("reviews unavailable"),
        errors.map(_.path) == List(
          List(PathValue.Key("products"), PathValue.Index(0), PathValue.Key("reviews"))
        ),
        errors.flatMap(_.extensions).exists(_.fields.contains("code" -> StringValue("REVIEWS_DOWN")))
      )
    },
    test("rejects invalid ordinary lookup metadata during gateway build") {
      val endpoint        = sttp.model.Uri.unsafeParse("http://127.0.0.1:1/graphql")
      val missingKey      = Lookup.list(
        "Product",
        List("missing"),
        "productsByRefs",
        Map("refs" -> Lookup.Argument.batch(Lookup.Argument.obj("productId" -> Lookup.Argument.key("missing")))),
        Lookup.Correlation.ordered
      )
      val wrongShape      = Lookup.single("Product", keyFields, "productsByRefs", Map("refs" -> refArgument))
      val missingBatch    = Lookup.list(
        "Product",
        keyFields,
        "productsByRefs",
        Map("refs" -> refArgument),
        Lookup.Correlation.ordered
      )
      val badCorrelation  = Lookup.list(
        "Product",
        keyFields,
        "productsByRefs",
        Map("refs" -> Lookup.Argument.batch(refArgument)),
        Lookup.Correlation.byKey(Map("missing" -> "id"))
      )
      val unknownArgument = Lookup.single(
        "Product",
        keyFields,
        "productByRef",
        Map("missing" -> refArgument)
      )
      val singleBatch     = Lookup.single(
        "Product",
        keyFields,
        "productByRef",
        Map("ref" -> Lookup.Argument.batch(refArgument))
      )
      val keyOutsideBatch = Lookup.list(
        "Product",
        keyFields,
        "productsByRefs",
        Map(
          "refs" -> Lookup.Argument.obj(
            "productId"  -> Lookup.Argument.key("id"),
            "regionCode" -> Lookup.Argument.batch(Lookup.Argument.key("region"))
          )
        ),
        Lookup.Correlation.ordered
      )
      val wrongTypes      = Lookup.single(
        "Product",
        keyFields,
        "productByRef",
        Map(
          "ref" -> Lookup.Argument.obj(
            "productId"  -> Lookup.Argument.key("region"),
            "regionCode" -> Lookup.Argument.key("id")
          )
        )
      )
      val nestedBatch     = Lookup.list(
        "Product",
        keyFields,
        "productsByRefs",
        Map("refs" -> Lookup.Argument.batch(Lookup.Argument.batch(refArgument))),
        Lookup.Correlation.ordered
      )
      val partialKeys     = Lookup.single(
        "Product",
        keyFields,
        "productByRef",
        Map(
          "ref" -> Lookup.Argument.obj(
            "productId"  -> Lookup.Argument.key("id"),
            "regionCode" -> Lookup.Argument.key("id")
          )
        )
      )

      def buildDiagnostics(lookup: Lookup, schema: String = reviewsSchema) =
        Gateway
          .compose(
            Subgraph.graphql("products", endpoint, productsSchema),
            Subgraph.graphql("reviews", endpoint, schema).withLookup(lookup)
          )
          .build
          .either
          .map(_.fold(_.diagnostics, _ => Nil))

      for {
        key         <- buildDiagnostics(missingKey)
        shape       <- buildDiagnostics(wrongShape)
        batch       <- buildDiagnostics(missingBatch)
        correlation <- buildDiagnostics(badCorrelation)
        nullable    <- buildDiagnostics(keyedLookup, reviewsSchema.replace("[Product!]!", "[Product]!"))
        unknown     <- buildDiagnostics(unknownArgument)
        single      <- buildDiagnostics(singleBatch)
        outside     <- buildDiagnostics(keyOutsideBatch)
        types       <- buildDiagnostics(wrongTypes)
        nested      <- buildDiagnostics(nestedBatch)
        coverage    <- buildDiagnostics(partialKeys)
        nonScalar   <- buildDiagnostics(keyedLookup, reviewsSchema.replace("region: String!", "region: Review!"))
        listKey     <- buildDiagnostics(keyedLookup, reviewsSchema.replace("region: String!", "region: [String!]!"))
        duplicate   <- Gateway
                         .compose(
                           Subgraph.graphql("products", endpoint, productsSchema),
                           Subgraph
                             .graphql("reviews", endpoint, reviewsSchema)
                             .withLookup(keyedLookup)
                             .withLookup(orderedLookup)
                         )
                         .build
                         .either
                         .map(_.fold(_.diagnostics, _ => Nil))
        federation  <- Gateway
                         .compose(
                           Subgraph.graphql("products", endpoint, productsSchema),
                           Subgraph.federation("reviews", endpoint, reviewsSchema).withLookup(keyedLookup)
                         )
                         .build
                         .either
                         .map(_.fold(_.diagnostics, _ => Nil))
      } yield assertTrue(
        key.exists(_.contains("[reviews] Lookup key field 'Product.missing' does not exist")),
        shape.exists(_.contains("[reviews] Lookup field 'Query.productsByRefs' must return 'Product'")),
        batch.exists(_.contains("[reviews] List lookup argument mappings must contain a batch mapping")),
        correlation.exists(_.contains("[reviews] Lookup correlation field 'Product.missing' does not exist")),
        nullable.exists(_.contains("[reviews] By-key lookup field 'Query.productsByRefs' must return non-null items")),
        unknown.exists(_.contains("[reviews] Lookup field 'Query.productByRef' has no argument 'missing'")),
        unknown.exists(_.contains("[reviews] Required lookup argument 'productByRef.ref' has no mapping")),
        single.exists(_.contains("[reviews] Single lookup argument mappings cannot contain a batch mapping")),
        outside.exists(_.contains("[reviews] List lookup key mappings must be nested inside a batch mapping")),
        types.count(_.contains("is incompatible with key field")) == 2,
        nested.exists(_.contains("[reviews] Lookup argument 'refs' cannot nest a batch mapping")),
        coverage.exists(_.contains("[reviews] Lookup argument mappings must use every declared key field")),
        nonScalar.exists(_.contains("[reviews] Lookup key field 'Product.region' must be a scalar or enum")),
        listKey.exists(_.contains("[reviews] Lookup key field 'Product.region' must be a scalar or enum")),
        duplicate.exists(_.contains("[reviews] More than one lookup is declared for type 'Product'")),
        federation.exists(_.contains("[reviews] Ordinary GraphQL lookups cannot be declared on a Federation subgraph"))
      )
    }
  ).provideSomeShared[zio.Scope](testServer, stubIds) @@ TestAspect.sequential

  private def reviewBody(value: caliban.ResponseValue): Option[caliban.ResponseValue] =
    field(value, "reviews").collect { case ListValue(review :: _) => review }.flatMap(field(_, "body"))
}
