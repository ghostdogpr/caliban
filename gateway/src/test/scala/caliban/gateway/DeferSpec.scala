package caliban.gateway

import caliban.ResponseValue.StreamValue
import caliban.federation.EntityResolver
import caliban.federation.v2_6.{ federated, GQLKey }
import caliban.schema.Annotations.GQLNonNullable
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, CalibanError, GraphQLRequest, GraphQLResponse, HttpUtils, InputValue, RootResolver }
import zio._
import zio.query.ZQuery
import zio.stream.ZStream
import zio.test._

object DeferSpec extends ZIOSpecDefault {

  private final class Fixture(val gate: Promise[Nothing, Unit], val calls: Ref[List[String]]) {
    def record(call: String): UIO[Unit] = calls.update(call :: _)
  }

  private object ProductsApi extends GenericSchema[Fixture] {
    import auto._

    sealed trait Item
    final case class Product(id: String, name: String, details: Details, slowName: URIO[Fixture, String]) extends Item
    final case class Coupon(code: String)                                                                 extends Item
    final case class Details(
      color: String,
      weight: Int,
      broken: IO[CalibanError, String],
      @GQLNonNullable brokenRequired: IO[CalibanError, String]
    )
    final case class ProductArgs(id: String)
    final case class RenameArgs(id: String, name: String)
    final case class Query(
      product: ProductArgs => URIO[Fixture, Option[Product]],
      products: URIO[Fixture, List[Product]],
      items: List[Item],
      slow: URIO[Fixture, String]
    )
    final case class Mutation(rename: RenameArgs => URIO[Fixture, Product])

    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen
    implicit val renameArgsSchema: Schema[Any, RenameArgs]   = Schema.gen
    implicit val renameArgsBuilder: ArgBuilder[RenameArgs]   = ArgBuilder.gen
    implicit val detailsSchema: Schema[Any, Details]         = Schema.gen
    implicit val productSchema: Schema[Fixture, Product]     =
      obj("Product", directives = List(GQLKey("id").directive))(implicit attributes =>
        List(
          field("id")(_.id),
          field("name")(_.name),
          field("details")(_.details),
          field("slowName")(_.slowName)
        )
      )
    implicit val itemSchema: Schema[Fixture, Item]           = gen
    implicit val querySchema: Schema[Fixture, Query]         = gen
    implicit val mutationSchema: Schema[Fixture, Mutation]   = gen

    private def product(id: String, name: String): Product =
      Product(
        id,
        name,
        Details(
          "oak",
          12,
          ZIO.fail(CalibanError.ExecutionError("broken")),
          ZIO.fail(CalibanError.ExecutionError("broken"))
        ),
        ZIO.serviceWithZIO[Fixture](fixture => fixture.gate.await.as(s"slow $name"))
      )

    private def names(id: String): String = if (id == "1") "Table" else "Chair"

    val api = graphQL(
      RootResolver(
        Query(
          args =>
            ZIO.serviceWithZIO[Fixture](_.record(s"product:${args.id}")).as(Some(product(args.id, names(args.id)))),
          ZIO.serviceWithZIO[Fixture](_.record("products")).as(List(product("1", "Table"), product("2", "Chair"))),
          List(product("1", "Table"), Coupon("SAVE")),
          ZIO.serviceWithZIO[Fixture](fixture => fixture.record("slow") *> fixture.gate.await).as("done")
        ),
        Mutation(args => ZIO.serviceWithZIO[Fixture](_.record(s"rename:${args.id}")).as(product(args.id, args.name)))
      )
    ) @@ federated(
      EntityResolver.from[ProductArgs](args =>
        ZQuery
          .fromZIO(ZIO.serviceWithZIO[Fixture](_.record(s"lookup:${args.id}")))
          .as(Some(product(args.id, names(args.id))))
      )
    )
  }

  private object ReviewsApi extends GenericSchema[Fixture] {
    import auto._

    final case class Product(id: String, reviews: List[Review])
    final case class Review(body: String, @GQLNonNullable score: IO[CalibanError, Int])
    final case class ProductArgs(id: String)
    final case class Query(reviewCount: Int)

    implicit val productArgsSchema: Schema[Any, ProductArgs] = Schema.gen
    implicit val productArgsBuilder: ArgBuilder[ProductArgs] = ArgBuilder.gen

    implicit val reviewSchema: Schema[Any, Review]   = Schema.gen
    implicit val querySchema: Schema[Any, Query]     = Schema.gen
    implicit val productSchema: Schema[Any, Product] =
      obj("Product", directives = List(GQLKey("id").directive))(implicit attributes =>
        List(field("id")(_.id), field("reviews")(_.reviews))
      )

    val api = graphQL(RootResolver(Query(2))) @@ federated(
      EntityResolver.from[ProductArgs](args =>
        ZQuery
          .fromZIO(ZIO.serviceWithZIO[Fixture](_.record(s"reviews:${args.id}")))
          .as(
            Some(
              Product(
                args.id,
                List(
                  Review(if (args.id == "1") "Solid" else "Wobbly", ZIO.fail(CalibanError.ExecutionError("no score")))
                )
              )
            )
          )
      )
    )
  }

  private val gateway: Gateway[Fixture] =
    Gateway.compose(Subgraph.federation("products", ProductsApi.api), Subgraph.federation("reviews", ReviewsApi.api))

  private def fixture: UIO[Fixture] =
    Promise.make[Nothing, Unit].zipWith(Ref.make(List.empty[String]))(new Fixture(_, _))

  private def stream(response: GraphQLResponse[CalibanError]): ZStream[Any, Throwable, caliban.ResponseValue] =
    response.data match {
      case StreamValue(stream) => stream.via(HttpUtils.DeferMultipart.createPipeline(response))
      case _                   => ZStream.succeed(response.toResponseValue)
    }

  private def run(
    query: String,
    variables: Map[String, InputValue] = Map.empty,
    gateway: Gateway[Fixture] = DeferSpec.gateway,
    open: Boolean = true
  ): ZIO[Scope, Throwable, (List[String], List[String])] =
    for {
      fixture  <- fixture
      _        <- fixture.gate.succeed(()).when(open)
      runtime  <- gateway.interpreter
      response <- runtime
                    .executeRequest(GraphQLRequest(query = Some(query), variables = Some(variables)))
                    .provideEnvironment(ZEnvironment(fixture))
      results  <- stream(response).runCollect
      calls    <- fixture.calls.get
    } yield (results.map(_.toString).toList, calls.reverse)

  override def spec = suite("DeferSpec")(
    test("defers fields resolved by another subgraph") {
      run("""{ product(id: "1") { name ... @defer(label: "reviews") { reviews { body } } } }""").map {
        case (results, calls) =>
          assertTrue(
            results == List(
              """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
              """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"],"label":"reviews"}],"hasNext":true}""",
              """{"hasNext":false}"""
            ),
            calls == List("product:1", "reviews:1")
          )
      }
    },
    test("defers named fragment spreads") {
      run(
        """{ product(id: "1") { name ...Reviews @defer(label: "reviews") } } fragment Reviews on Product { reviews { body } }"""
      ).map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
            """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"],"label":"reviews"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    },
    test("defers fragments with a type condition only for matching objects") {
      run("""{ items { ... on Product @defer { reviews { body } } ... on Coupon { code } } }""").map {
        case (results, _) =>
          assertTrue(
            results == List(
              """{"data":{"items":[{},{"code":"SAVE"}]},"hasNext":true}""",
              """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["items",0]}],"hasNext":true}""",
              """{"hasNext":false}"""
            )
          )
      }
    },
    test("stops null propagation at the deferred fragment") {
      run("""{ product(id: "1") { name ... @defer { reviews { body score } } } }""").map { case (results, _) =>
        assertTrue(
          results.head == """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
          results(1).startsWith("""{"incremental":[{"data":null,"errors":["""),
          results(1).endsWith(""""path":["product"]}],"hasNext":true}"""),
          results.last == """{"hasNext":false}"""
        )
      }
    },
    test("sends the initial payload before a deferred field of the same subgraph resolves") {
      for {
        fixture  <- fixture
        runtime  <- gateway.interpreter
        response <- runtime
                      .execute("""{ product(id: "1") { name ... @defer { slowName } } }""")
                      .provideEnvironment(ZEnvironment(fixture))
        first    <- Promise.make[Nothing, String]
        all      <- stream(response).tap(value => first.succeed(value.toString)).runCollect.fork
        initial  <- first.await.timeoutFail(new Exception("initial payload blocked"))(5.seconds)
        _        <- fixture.gate.succeed(())
        results  <- all.join
        calls    <- fixture.calls.get
      } yield assertTrue(
        initial == """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
        results.map(_.toString).toList.drop(1) == List(
          """{"incremental":[{"data":{"slowName":"slow Table"},"path":["product"]}],"hasNext":true}""",
          """{"hasNext":false}"""
        ),
        calls.reverse == List("product:1", "lookup:1")
      )
    } @@ TestAspect.withLiveClock,
    test("delivers deferred fields of a value type with their parent's payload") {
      for {
        fields   <- run("""{ product(id: "1") { name details { weight ... @defer { color } } } }""")
        nullable <- run("""{ product(id: "1") { name details { weight ... @defer { broken } } } }""")
        required <- run("""{ product(id: "1") { name details { weight ... @defer { brokenRequired } } } }""")
      } yield assertTrue(
        fields._1 == List("""{"data":{"product":{"name":"Table","details":{"weight":12,"color":"oak"}}}}"""),
        fields._2 == List("product:1"),
        nullable._1.size == 1,
        nullable._1.head.startsWith(
          """{"data":{"product":{"name":"Table","details":{"weight":12,"broken":null}}},"errors":[{"message":"broken""""
        ),
        required._1.size == 1,
        required._1.head.startsWith("""{"data":{"product":null},"errors":[{"message":"broken"""")
      )
    },
    test("defers root fields") {
      run("""{ product(id: "1") { name } ... @defer(label: "slow") { slow } }""").map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
            """{"incremental":[{"data":{"slow":"done"},"path":[],"label":"slow"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    },
    test("keeps root meta fields of a deferred fragment in the initial payload") {
      run("""{ product(id: "1") { name } ... @defer { __typename slow } }""").map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{"name":"Table"},"__typename":"Query"},"hasNext":true}""",
            """{"incremental":[{"data":{"slow":"done"},"path":[]}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    },
    test("resolves a label given as a variable") {
      run(
        """query($label: String) { product(id: "1") { name ... @defer(label: $label) { reviews { body } } } }""",
        Map("label" -> caliban.Value.StringValue("reviews"))
      ).map { case (results, _) =>
        assertTrue(
          results(1) ==
            """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"],"label":"reviews"}],"hasNext":true}"""
        )
      }
    },
    test("sends one entry per list item") {
      run("""{ products { id ... @defer { reviews { body } } } }""").map { case (results, calls) =>
        assertTrue(
          results == List(
            """{"data":{"products":[{"id":"1"},{"id":"2"}]},"hasNext":true}""",
            """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["products",0]},{"data":{"reviews":[{"body":"Wobbly"}]},"path":["products",1]}],"hasNext":true}""",
            """{"hasNext":false}"""
          ),
          calls.sorted == List("products", "reviews:1", "reviews:2")
        )
      }
    },
    test("sends nested deferred fragments after their parent") {
      run(
        """{ product(id: "1") { ... @defer(label: "outer") { name ... @defer(label: "inner") { reviews { body } } } } }"""
      ).map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{}},"hasNext":true}""",
            """{"incremental":[{"data":{"name":"Table"},"path":["product"],"label":"outer"}],"hasNext":true}""",
            """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"],"label":"inner"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    },
    test("delivers fields inline when @defer is disabled") {
      for {
        literal  <- run("""{ product(id: "1") { name ... @defer(if: false) { reviews { body } } } }""")
        disabled <-
          run(
            """query($defer: Boolean!) { product(id: "1") { name ... @defer(if: $defer) { reviews { body } } } }""",
            Map("defer" -> caliban.Value.BooleanValue(false))
          )
        enabled  <-
          run(
            """query($defer: Boolean!) { product(id: "1") { name ... @defer(if: $defer) { reviews { body } } } }""",
            Map("defer" -> caliban.Value.BooleanValue(true))
          )
      } yield assertTrue(
        literal._1 == List("""{"data":{"product":{"name":"Table","reviews":[{"body":"Solid"}]}}}"""),
        disabled._1 == literal._1,
        enabled._1.size == 3
      )
    },
    test("delivers fields also selected outside the deferred fragment in the initial payload") {
      run("""{ product(id: "1") { ... @defer { name reviews { body } } name } }""").map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
            """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"]}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    },
    test("delivers fields of an outer deferred fragment also selected in a nested one with the outer fragment") {
      val inner = """... @defer(label: "inner") { reviews { body } }"""
      for {
        innerFirst <- run(s"""{ product(id: "1") { ... @defer(label: "outer") { $inner reviews { body } } } }""")
        outerFirst <- run(s"""{ product(id: "1") { ... @defer(label: "outer") { reviews { body } $inner } } }""")
      } yield assertTrue(
        innerFirst._1 == List(
          """{"data":{"product":{}},"hasNext":true}""",
          """{"incremental":[{"data":{"reviews":[{"body":"Solid"}]},"path":["product"],"label":"outer"}],"hasNext":true}""",
          """{"hasNext":false}"""
        ),
        outerFirst._1 == innerFirst._1
      )
    },
    test("delivers the selection of a field also selected outside the deferred fragment in the initial payload") {
      run("""{ product(id: "1") { ... @defer { details { color } } details { weight } } }""").map { case (results, _) =>
        assertTrue(results == List("""{"data":{"product":{"details":{"color":"oak","weight":12}}}}"""))
      }
    },
    test("rejects cyclic fragments inside a deferred fragment") {
      run("""{ ...A } fragment A on Query { ... @defer { ...A } }""").map { case (results, _) =>
        assertTrue(results == List("""{"data":null,"errors":[{"message":"Fragment 'A' forms a cycle."}]}"""))
      }
    },
    test("defers fragments spread twice at each level of nesting") {
      def query(root: String, spreads: Int => String) =
        s"{ $root } " + (0 until 40).map(k => s"fragment F$k on Query { ${spreads(k)} }").mkString(" ") +
          """ fragment F40 on Query { product(id: "1") { name } }"""

      for {
        repeated <- run(query("... @defer(label: \"all\") { ...F0 }", k => s"...F${k + 1} ...F${k + 1}"))
        labeled  <-
          run(query("...F0", k => s"""...F${k + 1} @defer(label: "a$k") ...F${k + 1} @defer(label: "b$k")"""))
      } yield assertTrue(
        repeated._1 == List(
          """{"data":{},"hasNext":true}""",
          """{"incremental":[{"data":{"product":{"name":"Table"}},"path":[],"label":"all"}],"hasNext":true}""",
          """{"hasNext":false}"""
        ),
        labeled._1 == List(
          """{"data":{},"hasNext":true}""",
          """{"incremental":[{"data":{"product":{"name":"Table"}},"path":[],"label":"a39"}],"hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    } @@ TestAspect.timeout(30.seconds),
    test("ignores excluded spreads when sharing fragments inside deferred fragments") {
      def query(skip: String) =
        s"""{ product(id: "1") { ... @defer(label: "skipped") @skip(if: $skip) { ...Name } ... @defer(label: "kept") { ...Name } } } fragment Name on Product { name }"""
      for {
        literal  <- run(query("true"))
        variable <- run(s"query($$skip: Boolean!) ${query("$skip")}", Map("skip" -> caliban.Value.BooleanValue(true)))
      } yield assertTrue(
        literal._1 == List(
          """{"data":{"product":{}},"hasNext":true}""",
          """{"incremental":[{"data":{"name":"Table"},"path":["product"],"label":"kept"}],"hasNext":true}""",
          """{"hasNext":false}"""
        ),
        variable._1 == literal._1
      )
    },
    test("runs a mutation once when its result has deferred fields") {
      run("""mutation { rename(id: "1", name: "Desk") { name ... @defer { slowName reviews { body } } } }""").map {
        case (results, calls) =>
          assertTrue(
            results == List(
              """{"data":{"rename":{"name":"Desk"}},"hasNext":true}""",
              """{"incremental":[{"data":{"slowName":"slow Table","reviews":[{"body":"Solid"}]},"path":["rename"]}],"hasNext":true}""",
              """{"hasNext":false}"""
            ),
            calls.count(_.startsWith("rename")) == 1
          )
      }
    },
    test("rejects @defer on mutation root fields and duplicate labels") {
      for {
        root   <- run("""mutation { ... @defer { rename(id: "1", name: "Desk") { name } } }""")
        labels <-
          run(
            """{ product(id: "1") { ... @defer(label: "a") { name } ... @defer(label: "a") { details { color } } } }"""
          )
      } yield assertTrue(
        root._1.exists(_.contains("Stream or defer directive was used on a root field in a mutation or subscription")),
        labels._1.exists(_.contains("Stream and defer directive labels must be unique"))
      )
    },
    test("declares @defer but not @stream") {
      for {
        directives <- run("""{ __schema { directives { name } } }""")
        stream     <- run("""{ products @stream { id } }""")
      } yield assertTrue(
        directives._1.exists(_.contains("\"defer\"")),
        !directives._1.exists(_.contains("\"stream\"")),
        stream._1.exists(_.contains("stream"))
      )
    },
    test("explains deferred fragments") {
      for {
        fixture   <- fixture
        runtime   <- gateway.interpreter
        explained <- runtime
                       .explain("""{ product(id: "1") { name ... @defer(label: "reviews") { reviews { body } } } }""")
                       .provideEnvironment(ZEnvironment(fixture))
      } yield assertTrue(
        explained ==
          """query
            |fetch products at $.product fields [name, id (key)]
            |defer reviews at $.product fields [reviews.body]
            |  fetch reviews after products at $.product via Product(id) fields [reviews.body]""".stripMargin
      )
    },
    test("reports deferred fragments that exceed the request timeout") {
      run(
        """{ product(id: "1") { name ... @defer { slowName } } }""",
        gateway = gateway.withConfig(_.withRequestTimeout(200.millis)),
        open = false
      ).map { case (results, _) =>
        assertTrue(
          results == List(
            """{"data":{"product":{"name":"Table"}},"hasNext":true}""",
            """{"incremental":[{"data":null,"errors":[{"message":"Gateway request timed out.","path":["product"]}],"path":["product"]}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      }
    } @@ TestAspect.withLiveClock
  ) @@ TestAspect.timeout(60.seconds)
}
