package caliban.gateway

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import zio._
import zio.test._

object RoutePlanningSpec extends ZIOSpecDefault {

  private val rootSchema =
    s"""
       |${federationSchemaPreamble("@key", "@shareable")}
       |type Query { product: Product }
       |type Product @key(fields: "id") { id: ID! @shareable }
       |""".stripMargin

  private val costlyOwnerSchema =
    s"""
       |${federationSchemaPreamble("@key", "@external", "@requires", "@shareable")}
       |type Product @key(fields: "id") {
       |  id: ID! @external
       |  cost: Int! @external
       |  label: String! @shareable @requires(fields: "cost")
       |}
       |""".stripMargin

  private val directOwnerSchema =
    s"""
       |${federationSchemaPreamble("@key", "@external", "@shareable")}
       |type Product @key(fields: "id") {
       |  id: ID! @external
       |  label: String! @shareable
       |}
       |""".stripMargin

  private val costSchema =
    s"""
       |${federationSchemaPreamble("@key", "@shareable")}
       |type Product @key(fields: "id") {
       |  id: ID! @shareable
       |  cost: Int!
       |}
       |""".stripMargin

  private val shareableReplicaSchema =
    s"""
       |${federationSchemaPreamble("@key", "@shareable")}
       |type Product @key(fields: "id") { id: ID! @shareable }
       |""".stripMargin

  def spec = suite("Route planning")(
    test("chooses a complete field owner with fewer dependent calls") {
      for {
        root       <- stub(
                        """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                      )
        costly     <- stub("""{"data":{"_entities":[{"label":"costly"}]}}""")
        direct     <- stub("""{"data":{"_entities":[{"label":"direct"}]}}""")
        costs      <- stub("""{"data":{"_entities":[{"cost":10}]}}""")
        runtime    <- Gateway
                        .compose(
                          Subgraph.federation("a-root", root.endpoint, rootSchema),
                          Subgraph.federation("b-costly", costly.endpoint, costlyOwnerSchema),
                          Subgraph.federation("c-direct", direct.endpoint, directOwnerSchema),
                          Subgraph.federation("d-cost", costs.endpoint, costSchema)
                        )
                        .interpreter
        plan       <- runtime.explain("{ product { label } }")
        response   <- runtime.execute("{ product { label } }")
        costlySent <- costly.requests.get
        directSent <- direct.requests.get
        costSent   <- costs.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("direct")),
        plan.linesIterator.count(_.startsWith("fetch ")) == 2,
        plan.contains("fetch c-direct after a-root"),
        costlySent.isEmpty,
        directSent.size == 1,
        costSent.isEmpty
      )
    },
    test("uses a stable source-name tie-break for equivalent routes") {
      for {
        root     <- stub(
                      """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                    )
        zOwner   <- stub("""{"data":{"_entities":[{"label":"z"}]}}""")
        aOwner   <- stub("""{"data":{"_entities":[{"label":"a"}]}}""")
        runtime  <- Gateway
                      .compose(
                        Subgraph.federation("root", root.endpoint, rootSchema),
                        Subgraph.federation("z-owner", zOwner.endpoint, directOwnerSchema),
                        Subgraph.federation("a-owner", aOwner.endpoint, directOwnerSchema)
                      )
                      .interpreter
        plan     <- runtime.explain("{ product { label } }")
        response <- runtime.execute("{ product { label } }")
        aSent    <- aOwner.requests.get
        zSent    <- zOwner.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("a")),
        plan.contains("fetch a-owner after root"),
        aSent.size == 1,
        zSent.isEmpty
      )
    },
    test("chooses the usable Federation key with fewer internal selections") {
      val productsSchema =
        s"""
           |${federationSchemaPreamble("@key")}
           |type Query { product: Product }
           |type Product @key(fields: "id") { id: ID! sku: ID! upc: ID! }
           |""".stripMargin
      val reviewsSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@external")}
           |type Product @key(fields: "sku upc") @key(fields: "id") {
           |  id: ID! @external
           |  sku: ID! @external
           |  upc: ID! @external
           |  reviews: [Review!]!
           |}
           |type Review { body: String! }
           |""".stripMargin

      for {
        products <- stub(
                      """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                    )
        reviews  <- stub("""{"data":{"_entities":[{"reviews":[{"body":"good"}]}]}}""")
        runtime  <- Gateway
                      .compose(
                        Subgraph.federation("products", products.endpoint, productsSchema),
                        Subgraph.federation("reviews", reviews.endpoint, reviewsSchema)
                      )
                      .interpreter
        plan     <- runtime.explain("{ product { reviews { body } } }")
        response <- runtime.execute("{ product { reviews { body } } }")
        sent     <- reviews.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        plan.contains("via Product(id)"),
        !plan.contains("via Product(sku,upc)"),
        sent.headOption
          .flatMap(_.variables)
          .flatMap(_.get("representations"))
          .exists {
            case InputListValue(InputObjectValue(fields) :: Nil) =>
              fields.get("id").contains(StringValue("p1")) && !fields.contains("sku") && !fields.contains("upc")
            case _                                               => false
          }
      )
    },
    test("chooses the cheaper ordinary GraphQL lookup provider") {
      val productsSchema =
        """
          |type Query { products: [Product!]! }
          |type Product { id: ID! region: String! }
          |""".stripMargin
      val lookupSchema   =
        """
          |type Query { productsByIds(ids: [ID!]!): [Product!]! }
          |type Product { id: ID! reviews: [Review!]! }
          |type Review { body: String! }
          |""".stripMargin
      val compoundSchema =
        """
          |input Ref { id: ID! region: String! }
          |type Query { productsByRefs(refs: [Ref!]!): [Product!]! }
          |type Product { id: ID! region: String! reviews: [Review!]! }
          |type Review { body: String! }
          |""".stripMargin
      val byId           = Lookup.list(
        "Product",
        List("id"),
        "productsByIds",
        Map("id" -> "id"),
        "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id"))
      )
      val byRef          = Lookup.list(
        "Product",
        List("id", "region"),
        "productsByRefs",
        Map("id" -> "id", "region" -> "region"),
        "refs" -> Lookup.Argument.batch(
          Lookup.Argument.obj(
            "id"     -> Lookup.Argument.key("id"),
            "region" -> Lookup.Argument.key("region")
          )
        )
      )

      for {
        products     <- stub(
                          """{"data":{"products":[{"_caliban_gateway_key":"p1"}]}}"""
                        )
        compound     <- stub("""{"data":{"_caliban_gateway_lookup":[]}}""")
        simple       <-
          stub(
            """{"data":{"_caliban_gateway_lookup":[{"_caliban_gateway_lookup_key":"p1","reviews":[{"body":"simple"}]}]}}"""
          )
        runtime      <- Gateway
                          .compose(
                            Subgraph.graphql("products", products.endpoint, productsSchema),
                            Subgraph.graphql("a-compound", compound.endpoint, compoundSchema).withLookup(byRef),
                            Subgraph.graphql("b-simple", simple.endpoint, lookupSchema).withLookup(byId)
                          )
                          .interpreter
        response     <- runtime.execute("{ products { reviews { body } } }")
        compoundSent <- compound.requests.get
        simpleSent   <- simple.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "products").exists {
          case ListValue(ObjectValue(product) :: Nil) =>
            product.collectFirst { case ("reviews", value) => value }.exists {
              case ListValue(ObjectValue(review) :: Nil) => review.contains("body" -> StringValue("simple"))
              case _                                     => false
            }
          case _                                      => false
        },
        compoundSent.isEmpty,
        simpleSent.size == 1
      )
    },
    test("chooses the intermediate subgraph that completes the route with fewer calls") {
      val originSchema            =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Query { thing: Thing }
           |type Thing @key(fields: "a") { a: ID! @shareable }
           |""".stripMargin
      val helperSchema            =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Thing @key(fields: "a") @key(fields: "x") {
           |  a: ID! @shareable
           |  x: ID! @shareable
           |}
           |""".stripMargin
      val longIntermediateSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Thing @key(fields: "x") @key(fields: "d") {
           |  x: ID! @shareable
           |  d: ID! @shareable
           |}
           |""".stripMargin
      val shortIntermediateSchema =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Thing @key(fields: "a") @key(fields: "d") {
           |  a: ID! @shareable
           |  d: ID! @shareable
           |}
           |""".stripMargin
      val targetSchema            =
        s"""
           |${federationSchemaPreamble("@key", "@external")}
           |type Thing @key(fields: "d") {
           |  d: ID! @external
           |  label: String!
           |}
           |""".stripMargin

      for {
        origin            <- stub(
                               """{"data":{"thing":{"_caliban_gateway_key":"a1","_caliban_gateway_typename":"Thing"}}}"""
                             )
        helper            <- stub("""{"data":{"_entities":[]}}""")
        longIntermediate  <- stub("""{"data":{"_entities":[]}}""")
        shortIntermediate <-
          stub(
            """{"data":{"_entities":[{"_caliban_gateway_key":"d1","_caliban_gateway_typename":"Thing"}]}}"""
          )
        target            <- stub("""{"data":{"_entities":[{"label":"short"}]}}""")
        runtime           <- Gateway
                               .compose(
                                 Subgraph.federation("a-origin", origin.endpoint, originSchema),
                                 Subgraph.federation("b-helper", helper.endpoint, helperSchema),
                                 Subgraph.federation("c-long", longIntermediate.endpoint, longIntermediateSchema),
                                 Subgraph.federation("d-short", shortIntermediate.endpoint, shortIntermediateSchema),
                                 Subgraph.federation("e-target", target.endpoint, targetSchema)
                               )
                               .interpreter
        plan              <- runtime.explain("{ thing { label } }")
        response          <- runtime.execute("{ thing { label } }")
        helperSent        <- helper.requests.get
        longSent          <- longIntermediate.requests.get
        shortSent         <- shortIntermediate.requests.get
        targetSent        <- target.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "thing").flatMap(field(_, "label")).contains(StringValue("short")),
        plan.linesIterator.count(_.startsWith("fetch ")) == 3,
        plan.contains("fetch d-short after a-origin"),
        plan.contains("fetch e-target after d-short"),
        helperSent.isEmpty,
        longSent.isEmpty,
        shortSent.size == 1,
        targetSent.size == 1
      )
    },
    test("selects compatible keys across roots so entity work batches into one call") {
      val firstSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Query { first: Product }
           |type Product @key(fields: "k1") @key(fields: "k2") {
           |  k1: ID! @shareable
           |  k2: ID! @shareable
           |}
           |""".stripMargin
      val secondSchema =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Query { second: Product }
           |type Product @key(fields: "k2") { k2: ID! @shareable }
           |""".stripMargin
      val targetSchema =
        s"""
           |${federationSchemaPreamble("@key", "@external")}
           |type Product @key(fields: "k1") @key(fields: "k2") {
           |  k1: ID! @external
           |  k2: ID! @external
           |  label: String!
           |}
           |""".stripMargin

      for {
        first    <- stub(
                      """{"data":{"first":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                    )
        second   <- stub(
                      """{"data":{"second":{"_caliban_gateway_key":"p2","_caliban_gateway_typename":"Product"}}}"""
                    )
        target   <-
          stub(
            """{"data":{"_entities":[{"_caliban_gateway_entity_key":"p1","label":"one"},{"_caliban_gateway_entity_key":"p2","label":"two"}]}}"""
          )
        runtime  <- Gateway
                      .compose(
                        Subgraph.federation("first", first.endpoint, firstSchema),
                        Subgraph.federation("second", second.endpoint, secondSchema),
                        Subgraph.federation("target", target.endpoint, targetSchema)
                      )
                      .interpreter
        plan     <- runtime.explain("{ first { label } second { label } }")
        response <- runtime.execute("{ first { label } second { label } }")
        sent     <- target.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "first").flatMap(field(_, "label")).contains(StringValue("one")),
        field(response.data, "second").flatMap(field(_, "label")).contains(StringValue("two")),
        plan.linesIterator.count(_.contains("via Product(k2)")) == 2,
        !plan.contains("via Product(k1)"),
        sent.size == 1,
        sent.headOption
          .flatMap(_.variables)
          .flatMap(_.get("representations"))
          .exists {
            case InputListValue(values) =>
              values.collect { case InputObjectValue(fields) => fields.get("k2") }.flatten ==
                List(StringValue("p1"), StringValue("p2")) &&
                values.forall {
                  case InputObjectValue(fields) => !fields.contains("k1")
                  case _                        => false
                }
            case _                      => false
          }
      )
    },
    test("discards a cyclic alternative when a complete dependency DAG exists") {
      val originSchema =
        """
          |type Query { thing: Thing }
          |type Thing { seed: ID! }
          |""".stripMargin
      val leftSchema   =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@shareable")}
           |type Thing @key(fields: "c") {
           |  b: ID! @shareable
           |  c: ID! @external
           |}
           |""".stripMargin
      val rightSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@shareable")}
           |type Thing @key(fields: "b") {
           |  b: ID! @external
           |  c: ID! @shareable
           |}
           |""".stripMargin
      val validSchema  =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@shareable")}
           |type Thing @key(fields: "seed") {
           |  seed: ID! @external
           |  b: ID! @shareable
           |  c: ID! @shareable
           |}
           |""".stripMargin

      for {
        origin    <- stub(
                       """{"data":{"thing":{"_caliban_gateway_key":"s1","_caliban_gateway_typename":"Thing"}}}"""
                     )
        left      <- stub("""{"data":{"_entities":[]}}""")
        right     <- stub("""{"data":{"_entities":[]}}""")
        valid     <- stub("""{"data":{"_entities":[{"b":"b1","c":"c1"}]}}""")
        runtime   <- Gateway
                       .compose(
                         Subgraph.graphql("origin", origin.endpoint, originSchema),
                         Subgraph.federation("left", left.endpoint, leftSchema),
                         Subgraph.federation("right", right.endpoint, rightSchema),
                         Subgraph.federation("valid", valid.endpoint, validSchema)
                       )
                       .interpreter
        response  <- runtime.execute("{ thing { b c } }")
        leftSent  <- left.requests.get
        rightSent <- right.requests.get
        validSent <- valid.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "thing").flatMap(field(_, "b")).contains(StringValue("b1")),
        field(response.data, "thing").flatMap(field(_, "c")).contains(StringValue("c1")),
        leftSent.isEmpty,
        rightSent.isEmpty,
        validSent.size == 1
      )
    },
    test("keeps unambiguous plans on the direct path") {
      for {
        root      <- stub(
                       """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
                     )
        owner     <- stub("""{"data":{"_entities":[{"label":"direct"}]}}""")
        runtime   <- Gateway
                       .compose(
                         Subgraph.federation("root", root.endpoint, rootSchema),
                         Subgraph.federation("owner", owner.endpoint, directOwnerSchema)
                       )
                       .withConfig(
                         _.withMaxPlanningCandidates(1)
                           .withMaxPlanningExpansions(1)
                       )
                       .interpreter
        response  <- runtime.execute("{ product { label } }")
        rootSent  <- root.requests.get
        ownerSent <- owner.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "label")).contains(StringValue("direct")),
        rootSent.size == 1,
        ownerSent.size == 1
      )
    },
    test("prefers a requirement-free local owner without expanding remote shareable alternatives") {
      for {
        root     <- stub("""{"data":{"product":{"id":"p1"}}}""")
        replica  <- stub("""{"data":{"_entities":[]}}""")
        runtime  <- Gateway
                      .compose(
                        Subgraph.federation("root", root.endpoint, rootSchema),
                        Subgraph.federation("replica", replica.endpoint, shareableReplicaSchema)
                      )
                      .withConfig(_.withMaxPlanningCandidates(1))
                      .interpreter
        response <- runtime.execute("{ product { id } }")
        rootSent <- root.requests.get
        remote   <- replica.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        field(response.data, "product").flatMap(field(_, "id")).contains(StringValue("p1")),
        rootSent.size == 1,
        remote.isEmpty
      )
    },
    test("plans invariant nested alternatives once within the candidate budget") {
      val rootsSchema    =
        s"""
           |${federationSchemaPreamble("@key")}
           |type Query { product: Product }
           |type Product @key(fields: "id") { id: ID! }
           |""".stripMargin
      val productsSchema =
        s"""
           |${federationSchemaPreamble("@key", "@external")}
           |type Product @key(fields: "id") {
           |  id: ID! @external
           |  one: One!
           |  two: Two!
           |  three: Three!
           |}
           |type One @key(fields: "id") { id: ID! }
           |type Two @key(fields: "id") { id: ID! }
           |type Three @key(fields: "id") { id: ID! }
           |""".stripMargin
      val valueSchema    =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@shareable")}
           |type One @key(fields: "id") { id: ID! @external value: String! @shareable }
           |type Two @key(fields: "id") { id: ID! @external value: String! @shareable }
           |type Three @key(fields: "id") { id: ID! @external value: String! @shareable }
           |""".stripMargin

      Gateway
        .compose(
          Subgraph.federation("roots", unreachableEndpoint, rootsSchema),
          Subgraph.federation("products", unreachableEndpoint, productsSchema),
          Subgraph.federation("values-a", unreachableEndpoint, valueSchema),
          Subgraph.federation("values-b", unreachableEndpoint, valueSchema)
        )
        .withConfig(_.withMaxPlanningCandidates(128))
        .interpreter
        .flatMap(_.explain("{ product { one { value } two { value } three { value } } }").exit)
        .map(exit => assertTrue(exit.isSuccess))
    },
    test("fails rather than dropping a fragment for an unresolved concrete type") {
      val rootsSchema   =
        s"""
           |${federationSchemaPreamble("@key", "@shareable")}
           |type Query { nodes: [Node!]! }
           |interface Node { id: ID! @shareable }
           |type A implements Node @key(fields: "id") { id: ID! @shareable }
           |type B implements Node @key(fields: "id") { id: ID! @shareable }
           |type C implements Node @key(fields: "id") { id: ID! @shareable }
           |""".stripMargin
      val detailsSchema =
        s"""
           |${federationSchemaPreamble("@key", "@external", "@shareable")}
           |interface Node { id: ID! @shareable }
           |type A implements Node @key(fields: "id") { id: ID! @external detail: String! }
           |type B implements Node @key(fields: "id") { id: ID! @external detail: String! }
           |type C implements Node @key(fields: "slug") {
           |  id: ID! @external
           |  slug: ID!
           |  detail: String!
           |}
           |""".stripMargin

      for {
        roots    <- stub("""{"data":{"nodes":[]}}""")
        details  <- stub("""{"data":{"_entities":[]}}""")
        gateway  <- Gateway
                      .compose(
                        Subgraph.federation("roots", roots.endpoint, rootsSchema),
                        Subgraph.federation("details", details.endpoint, detailsSchema)
                      )
                      .interpreter
        response <- gateway.execute("{ nodes { ... on C { detail } } }")
        rootSent <- roots.requests.get
        sent     <- details.requests.get
      } yield assertTrue(
        response.errors.nonEmpty,
        rootSent.isEmpty,
        sent.isEmpty
      )
    },
    test("fails safely before source work when planning guardrails are exhausted") {
      for {
        root             <- stub("""{"data":{"product":null}}""")
        costly           <- stub("""{"data":{"_entities":[]}}""")
        direct           <- stub("""{"data":{"_entities":[]}}""")
        costs            <- stub("""{"data":{"_entities":[]}}""")
        gateway           = Gateway.compose(
                              Subgraph.federation("a-root", root.endpoint, rootSchema),
                              Subgraph.federation("b-costly", costly.endpoint, costlyOwnerSchema),
                              Subgraph.federation("c-direct", direct.endpoint, directOwnerSchema),
                              Subgraph.federation("d-cost", costs.endpoint, costSchema)
                            )
        candidateRuntime <- gateway.withConfig(_.withMaxPlanningCandidates(1)).interpreter
        workRuntime      <- gateway.withConfig(_.withMaxPlanningExpansions(1)).interpreter
        timeoutRuntime   <- gateway.withConfig(_.withPlanningTimeout(1.nanosecond)).interpreter
        candidate        <- candidateRuntime.execute("{ product { label } }")
        work             <- workRuntime.execute("{ product { label } }")
        timeout          <- timeoutRuntime.execute("{ product { label } }")
        sent             <- ZIO.foreach(List(root, costly, direct, costs))(_.requests.get)
      } yield assertTrue(
        candidate.errors.map(_.msg) == List("Route planning exceeded the configured candidate limit."),
        work.errors.map(_.msg) == List("Route planning exceeded the configured expansion limit."),
        timeout.errors.map(_.msg) == List("Route planning exceeded the configured duration limit."),
        sent.forall(_.isEmpty)
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
