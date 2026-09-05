package caliban.gateway

import caliban.Value.StringValue
import caliban.Value.IntValue.IntNumber
import caliban.ResponseValue.{ ListValue => ResponseListValue }
import caliban.{ GraphQLRequest, InputValue }
import caliban.gateway.GatewayTestSupport._
import caliban.execution.{ Field, RequestPreparation }
import caliban.gateway.internal.composition.{ ComposedGraph, OperationCost }
import caliban.gateway.internal.planning.OperationPlan
import caliban.parsing.Parser
import caliban.parsing.adt.OperationType
import caliban.tools.RemoteSchema
import zio._
import zio.test._

object OperationCostSpec extends ZIOSpecDefault {

  private final class CountingCosts(values: Map[(String, String), Long])
      extends Map.WithDefault[(String, String), Long](values, values.default) {
    var lookups = 0

    override def get(key: (String, String)): Option[Long] = {
      lookups += 1
      values.get(key)
    }
  }

  private val costDefinition =
    "directive @cost(weight: Int!) on ARGUMENT_DEFINITION | ENUM | FIELD_DEFINITION | INPUT_FIELD_DEFINITION | OBJECT | SCALAR"

  private val listSizeDefinition =
    "directive @listSize(assumedSize: Int, slicingArguments: [String!], sizedFields: [String!], requireOneSlicingArgument: Boolean = true) on FIELD_DEFINITION"

  private val directives =
    """
      |directive @link(url: String!, as: String, import: [link__Import], for: link__Purpose) repeatable on SCHEMA
      |%s
      |directive @shareable repeatable on OBJECT | FIELD_DEFINITION
      |scalar link__Import
      |enum link__Purpose { SECURITY EXECUTION }
      |""".stripMargin.format(costDefinition)

  private def schema(costName: String = "cost", fieldWeight: Option[Int] = None): String = {
    val imported  = if (costName == "cost") "\"@cost\"" else s"""{ name: "@cost", as: "@$costName" }"""
    val fieldCost = fieldWeight.fold("")(weight => s" @$costName(weight: $weight)")
    s"""
       |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: [$imported]) { query: Query }
       |${directives.replace("@cost", s"@$costName")}
       |type Query { book: Book }
       |type Book { title: String address: Address$fieldCost }
       |type Address @$costName(weight: 5) { zipCode: Int }
       |""".stripMargin
  }

  private val query    = "{ book { title address { zipCode } } }"
  private val response = """{"data":{"book":{"title":"Caliban","address":{"zipCode":1}}}}"""

  private def code(error: caliban.CalibanError): Option[String] =
    error match {
      case value: caliban.CalibanError.ValidationError =>
        value.extensions.flatMap(_.fields.collectFirst { case ("code", StringValue(code)) => code })
      case value: caliban.CalibanError.ExecutionError  =>
        value.extensions.flatMap(_.fields.collectFirst { case ("code", StringValue(code)) => code })
      case _                                           => None
    }

  def spec = suite("OperationCostSpec")(
    test("does not multiply shared child cost work across nested runtime branches") {
      val names                           = List("A", "B", "C", "D")
      val schema                          = "type Query { node: Node } interface Node { child: Node value: String } " +
        names.map(name => s"type $name implements Node { child: Node value: String }").mkString(" ")
      def selection(depth: Int): String   =
        if (depth == 0) "value"
        else s"child { ${selection(depth - 1)} } " + names.map(name => s"... on $name { value }").mkString(" ")
      val query                           = s"{ node { ${selection(6)} } }"
      def count(fields: List[Field]): Int = fields.map(field => 1 + count(field.fields)).sum
      for {
        document  <- ZIO.fromEither(Parser.parseQuery(schema))
        root      <- ZIO.fromEither(RemoteSchema.toRootType(document))
        operation <- RequestPreparation.parse(query)
        request   <-
          RequestPreparation.prepareParsed(GraphQLRequest(query = Some(query)), operation, Map.empty, root, false)
        weights    = new CountingCosts(names.zipWithIndex.map { case (name, index) =>
                       (name -> "value") -> (index + 1L)
                     }.toMap)
        metadata   = ComposedGraph.CostMetadata(Map.empty, weights, Map.empty, Map.empty, Map.empty)
        costs      = new OperationCost(root.types, Map("Node" -> names.toSet), metadata)
        plan       = OperationPlan(OperationType.Query, "Query", request.field.fields, Nil, Nil, Nil, Nil, Some("nodes"))
        estimated  = costs.estimate(request, plan)
      } yield assertTrue(
        estimated == Right(35L),
        weights.lookups > 0,
        weights.lookups <= count(request.field.fields) * names.size * 2
      )
    },
    test("retains runtime conditions and aliases when overlapping fields have different children") {
      val schema  = """
                     |type Query { node: Node }
                     |interface Node { child: Child }
                     |type A implements Node { child: Child }
                     |type B implements Node { child: Child }
                     |type Child { cheap: String expensive: String }
                     |""".stripMargin
      val queries = List(
        "{ node { child { cheap } ... on A { child { expensive } } ... on B { child { cheap } } } }" -> 103L,
        "{ node { a: child { cheap } b: child { cheap } ... on A { a: child { expensive } } } }"     -> 105L
      )
      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        root     <- ZIO.fromEither(RemoteSchema.toRootType(document))
        metadata  = ComposedGraph.CostMetadata(
                      Map.empty,
                      Map(("Child" -> "cheap") -> 1L, ("Child" -> "expensive") -> 100L),
                      Map.empty,
                      Map.empty,
                      Map.empty
                    )
        costs     = new OperationCost(root.types, Map("Node" -> Set("A", "B")), metadata)
        results  <- ZIO.foreach(queries) { case (query, expected) =>
                      for {
                        operation <- RequestPreparation.parse(query)
                        request   <- RequestPreparation.prepareParsed(
                                       GraphQLRequest(query = Some(query)),
                                       operation,
                                       Map.empty,
                                       root,
                                       false
                                     )
                        plan       = OperationPlan(
                                       OperationType.Query,
                                       "Query",
                                       request.field.fields,
                                       Nil,
                                       Nil,
                                       Nil,
                                       Nil,
                                       Some("nodes")
                                     )
                      } yield assertTrue(costs.estimate(request, plan) == Right(expected))
                    }
      } yield results.reduce(_ && _)
    },
    test("charges omitted input-field defaults in literals and variables") {
      val inputSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: Query }
           |$directives
           |input Filter { expensive: String = "x" @cost(weight: 100) }
           |type Query { search(filter: Filter): String }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"search":"ok"}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("search", remote.endpoint, inputSchema))
                     .withConfig(_.withMaxOperationCost(10))
                     .interpreter
        results <- ZIO.foreach(
                     List(
                       GraphQLRequest(query = Some("{ search(filter: {}) }")),
                       GraphQLRequest(query = Some("{ search(filter: { expensive: \"x\" }) }")),
                       GraphQLRequest(
                         query = Some("query($filter: Filter) { search(filter: $filter) }"),
                         variables = Some(Map("filter" -> InputValue.ObjectValue(Map.empty)))
                       )
                     )
                   )(runtime.executeRequest(_))
        sent    <- remote.requests.get
      } yield assertTrue(
        results.forall(_.errors.map(_.msg) == List("Operation cost 101 exceeds the configured maximum of 10.")),
        sent.isEmpty
      )
    },
    test("collects duplicate and overlapping passthrough selections before charging cost") {
      for {
        remote  <- stub("""{"data":{"book":{"title":"Caliban"}}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, schema()))
                     .withConfig(_.withMaxOperationCost(1))
                     .interpreter
        results <- ZIO.foreach(
                     List(
                       "{ book { title } }",
                       "{ book { title } book { title } }",
                       "{ book { title } ...Books } fragment Books on Query { book { title } }",
                       "{ book { title ... on Book { title } } }"
                     )
                   )(runtime.execute(_))
        aliased <- runtime.execute("{ a: book { title } b: book { title } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        results.forall(_.errors.isEmpty),
        sent.size == 4,
        aliased.errors.map(_.msg) == List("Operation cost 2 exceeds the configured maximum of 1.")
      )
    },
    test("collects overlapping abstract-type fields once per runtime object") {
      val inputSchema = """
                          |type Query { node: Node }
                          |interface Node { book: Book }
                          |type Product implements Node { book: Book }
                          |type User implements Node { book: Book }
                          |type Book { title: String }
                          |""".stripMargin
      for {
        remote  <- stub("""{"data":{"node":null}}""")
        runtime <- Gateway
                     .compose(Subgraph.graphql("nodes", remote.endpoint, inputSchema))
                     .withConfig(_.withMaxOperationCost(2))
                     .interpreter
        result  <- runtime.execute(
                     "{ node { book { title } ... on Product { book { title } } ... on User { book { title } } } }"
                   )
        sent    <- remote.requests.get
      } yield assertTrue(result.errors.isEmpty, sent.size == 1)
    },
    test("charges nested defaults in lists without applying defaults to explicit null objects") {
      val inputSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: Query }
           |$directives
           |input Filter { expensive: String = "x" @cost(weight: 100) }
           |input Options { filters: [Filter!] = [{}] }
           |type Query { search(options: Options): String }
           |""".stripMargin
      for {
        remote    <- stub("""{"data":{"search":"ok"}}""")
        runtime   <- Gateway
                       .compose(Subgraph.federation("search", remote.endpoint, inputSchema))
                       .withConfig(_.withMaxOperationCost(10))
                       .interpreter
        defaulted <- runtime.execute("{ search(options: {}) }")
        supplied  <- runtime.execute("{ search(options: { filters: [{}] }) }")
        nulled    <- runtime.execute("{ search(options: null) }")
        omitted   <- runtime.execute("{ search }")
        sent      <- remote.requests.get
      } yield assertTrue(
        defaulted.errors.map(_.msg) == List("Operation cost 102 exceeds the configured maximum of 10."),
        supplied.errors.map(_.msg) == defaulted.errors.map(_.msg),
        nulled.errors.isEmpty,
        omitted.errors.isEmpty,
        sent.size == 2
      )
    },
    test("enforces type cost before contacting a subgraph") {
      for {
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, schema()))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute(query)
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.map(_.msg) == List("Operation cost 6 exceeds the configured maximum of 5."),
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("accepts an operation at the configured maximum") {
      for {
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, schema()))
                     .withConfig(_.withMaxOperationCost(6))
                     .interpreter
        result  <- runtime.execute(query)
        sent    <- remote.requests.get
      } yield assertTrue(result.errors.isEmpty, sent.size == 1)
    },
    test("adds field cost to return-type cost") {
      for {
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, schema(fieldWeight = Some(2))))
                     .withConfig(_.withMaxOperationCost(7))
                     .interpreter
        result  <- runtime.execute(query)
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("uses the maximum concrete field cost for an interface selection") {
      val interfaceSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: Query }
           |$directives
           |interface Node { id: ID! }
           |type Product implements Node { id: ID! @cost(weight: 5) }
           |type User implements Node { id: ID! @cost(weight: 2) }
           |type Query { node: Node }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"node":{"id":"p1"}}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("nodes", remote.endpoint, interfaceSchema))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute("{ node { id } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.map(_.msg) == List("Operation cost 6 exceeds the configured maximum of 5."),
        sent.isEmpty
      )
    },
    test("uses concrete argument costs for an interface selection") {
      val interfaceSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: Query }
           |$directives
           |input Filter { term: String }
           |interface Node { search(filter: Filter): String }
           |type Product implements Node { search(filter: Filter @cost(weight: 5)): String }
           |type User implements Node { search(filter: Filter @cost(weight: 2)): String }
           |type Query { node: Node }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"node":{"search":"result"}}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("nodes", remote.endpoint, interfaceSchema))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute("{ node { search(filter: { term: \"caliban\" }) } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.map(_.msg) == List("Operation cost 6 exceeds the configured maximum of 5."),
        sent.isEmpty
      )
    },
    test("supports an imported alias") {
      for {
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, schema("expensive")))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute(query)
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("adds argument and selected input-field costs") {
      val inputSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: Query }
           |$directives
           |input Filter { term: String @cost(weight: 4) ignored: String @cost(weight: 100) }
           |type Query { search(filter: [Filter!]! @cost(weight: 3)): String @cost(weight: 2) }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"search":"result"}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("search", remote.endpoint, inputSchema))
                     .withConfig(_.withMaxOperationCost(8))
                     .interpreter
        result  <- runtime.execute("{ search(filter: { term: \"caliban\" }) }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("multiplies a list selection by an assumed size") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query { books: [Book!]! @listSize(assumedSize: 3) }
           |type Book { author: Author }
           |type Author { name: String }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"books":[]}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute("{ books { author { name } } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("counts field and argument costs once for a list") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost", "@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  items(limit: Int @cost(weight: 2)): [Item!]!
           |    @cost(weight: 5)
           |    @listSize(assumedSize: 3)
           |}
           |type Item { name: String }
           |""".stripMargin
      for {
        remote          <- stub("""{"data":{"items":[]}}""")
        rejectedRuntime <- Gateway
                             .compose(Subgraph.federation("items", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(9))
                             .interpreter
        rejected        <- rejectedRuntime.execute("{ items(limit: 3) { name } }")
        acceptedRuntime <- Gateway
                             .compose(Subgraph.federation("items", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(10))
                             .interpreter
        accepted        <- acceptedRuntime.execute("{ items(limit: 3) { name } }")
        sent            <- remote.requests.get
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        accepted.errors.isEmpty,
        sent.size == 1
      )
    },
    test("uses schema defaults for slicing and cost arguments") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost", "@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  books(first: Int = 4): [Book!]! @listSize(slicingArguments: ["first"])
           |  search(filter: String = "all" @cost(weight: 5)): String
           |}
           |type Book { title: String }
           |""".stripMargin
      for {
        remote   <- stub("{}")
        runtime  <- Gateway
                      .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                      .withConfig(_.withMaxOperationCost(3))
                      .interpreter
        list     <- runtime.execute("{ books { title } }")
        argument <- runtime.execute("{ search }")
        sent     <- remote.requests.get
      } yield assertTrue(
        list.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        argument.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("uses the largest supplied slicing argument and clamps negative values") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  books(first: Int, last: Int): [Book!]!
           |    @listSize(slicingArguments: ["first", "last"], requireOneSlicingArgument: false)
           |}
           |type Book { title: String }
           |""".stripMargin
      for {
        remote   <- stub("""{"data":{"books":[]}}""", """{"data":{"books":[]}}""")
        runtime  <- Gateway
                      .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                      .withConfig(_.withMaxOperationCost(3))
                      .interpreter
        rejected <- runtime.execute("{ books(first: -10, last: 4) { title } }")
        accepted <- runtime.execute("{ books(first: -10) { title } }")
        sent     <- remote.requests.get
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        accepted.errors.isEmpty,
        sent.size == 1
      )
    },
    test("resolves nested and list-valued slicing arguments") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |input Pagination { first: Int }
           |input Search { pagination: Pagination }
           |type Query {
           |  search(input: Search): [Book!]! @listSize(slicingArguments: ["input.pagination.first"])
           |  byIds(ids: [ID!]!): [Book!]! @listSize(slicingArguments: ["ids"])
           |}
           |type Book { title: String }
           |""".stripMargin
      for {
        remote    <- stub("""{"data":{"byIds":[]}}""")
        runtime   <- Gateway
                       .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                       .withConfig(_.withMaxOperationCost(2))
                       .interpreter
        nested    <- runtime.executeRequest(
                       GraphQLRequest(
                         query = Some("query Search($input: Search) { search(input: $input) { title } }"),
                         variables = Some(
                           Map(
                             "input" -> InputValue.ObjectValue(
                               Map("pagination" -> InputValue.ObjectValue(Map("first" -> IntNumber(3))))
                             )
                           )
                         )
                       )
                     )
        list      <- runtime.execute("{ byIds(ids: [\"a\", \"b\", \"c\"]) { title } }")
        singleton <- runtime.execute("{ byIds(ids: \"a\") { title } }")
        sent      <- remote.requests.get
      } yield assertTrue(
        nested.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        list.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        singleton.errors.isEmpty,
        sent.size == 1
      )
    },
    test("requires exactly one slicing argument when configured") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  books(first: Int, last: Int): [Book!]! @listSize(slicingArguments: ["first", "last"])
           |}
           |type Book { title: String }
           |""".stripMargin
      for {
        remote    <- stub("""{"data":{"books":[]}}""")
        runtime   <- Gateway
                       .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                       .withConfig(_.withMaxOperationCost(100))
                       .interpreter
        missing   <- runtime.execute("{ books { title } }")
        duplicate <- runtime.execute("{ books(first: 1, last: 2) { title } }")
        sent      <- remote.requests.get
      } yield assertTrue(
        missing.errors.map(_.msg) == List("Exactly one slicing argument must be supplied for field 'Query.books'."),
        duplicate.errors.map(_.msg) == List("Exactly one slicing argument must be supplied for field 'Query.books'."),
        missing.errors.flatMap(code) == List("COST_QUERY_PARSE_FAILURE"),
        duplicate.errors.flatMap(code) == List("COST_QUERY_PARSE_FAILURE"),
        sent.isEmpty
      )
    },
    test("applies a list size only to configured sized fields") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  books(first: Int): Cursor! @listSize(slicingArguments: ["first"], sizedFields: ["results { page }"])
           |}
           |type Cursor { results: Results! recent: [Book!]! }
           |type Results { page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"books":{"results":{"page":[]},"recent":[]}}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute("{ books(first: 3) { results { page { title } } recent { title } } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("keeps nested sized paths when the same field is also sized directly") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  container: Container @listSize(assumedSize: 2, sizedFields: ["items", "items { parts }"])
           |}
           |type Container { items: [Item!]! }
           |type Item { parts: [Part!]! }
           |type Part { value: String }
           |""".stripMargin
      for {
        remote  <- stub("{}")
        runtime <- Gateway
                     .compose(Subgraph.federation("items", remote.endpoint, listSchema))
                     .withConfig(_.withMaxOperationCost(5))
                     .interpreter
        result  <- runtime.execute("{ container { items { parts { value } } } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("uses concrete argument costs on intermediate sized fields") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost", "@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  container: Container @listSize(assumedSize: 2, sizedFields: ["holder { page }"])
           |}
           |interface Container { holder(filter: String): Holder }
           |type ConcreteContainer implements Container {
           |  holder(filter: String @cost(weight: 5)): Holder
           |}
           |interface Holder { page: [Book!]! }
           |type ConcreteHolder implements Holder { page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        remote  <- stub("{}")
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                     .withConfig(_.withMaxOperationCost(8))
                     .interpreter
        result  <- runtime.execute("{ container { holder(filter: \"all\") { page { title } } } }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("prefers the nearest list size along a sized-field path") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  container: Cursor @listSize(assumedSize: 5, sizedFields: ["results { page }"])
           |}
           |type Cursor {
           |  results: Results @listSize(assumedSize: 2, sizedFields: ["page"])
           |}
           |type Results { page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        remote          <- stub("""{"data":{"container":{"results":{"page":[]}}}}""")
        rejectedRuntime <- Gateway
                             .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(3))
                             .interpreter
        rejected        <- rejectedRuntime.execute("{ container { results { page { title } } } }")
        acceptedRuntime <- Gateway
                             .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(4))
                             .interpreter
        accepted        <- acceptedRuntime.execute("{ container { results { page { title } } } }")
        sent            <- remote.requests.get
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        accepted.errors.isEmpty,
        sent.size == 1
      )
    },
    test("supports branching sized-field paths through list-valued fields") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  container: Container
           |    @listSize(assumedSize: 2, sizedFields: ["groups { items } featured { items }"])
           |}
           |type Container { groups: [Group!]! @listSize(assumedSize: 3) featured: Group }
           |type Group { items: [Item!]! }
           |type Item { name: String }
           |""".stripMargin
      val query      = "{ container { groups { items { name } } featured { items { name } } } }"
      for {
        remote          <- stub("""{"data":{"container":{"groups":[],"featured":{"items":[]}}}}""")
        rejectedRuntime <- Gateway
                             .compose(Subgraph.federation("items", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(12))
                             .interpreter
        rejected        <- rejectedRuntime.execute(query)
        acceptedRuntime <- Gateway
                             .compose(Subgraph.federation("items", remote.endpoint, listSchema))
                             .withConfig(_.withMaxOperationCost(13))
                             .interpreter
        accepted        <- acceptedRuntime.execute(query)
        sent            <- remote.requests.get
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        accepted.errors.isEmpty,
        sent.size == 1
      )
    },
    test("takes the maximum of mutually exclusive sized-field branches") {
      val listSchema =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query {
           |  container: Cursor @listSize(assumedSize: 2, sizedFields: ["node { page }"])
           |}
           |type Cursor { node: Node }
           |interface Node { id: ID! page: [Book!]! }
           |type Product implements Node { id: ID! page: [Book!]! }
           |type User implements Node { id: ID! page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        remote  <-
          stub(
            """{"data":{"container":{"node":{"_caliban_gateway_runtime_typename":"Product","__typename":"Product","page":[]}}}}"""
          )
        runtime <- Gateway
                     .compose(Subgraph.federation("books", remote.endpoint, listSchema))
                     .withConfig(_.withMaxOperationCost(4))
                     .interpreter
        result  <- runtime.execute(
                     "{ container { node { ... on Product { page { title } } ... on User { page { title } } } } }"
                   )
        sent    <- remote.requests.get
      } yield assertTrue(result.errors.isEmpty, sent.size == 1)
    },
    test("keeps cost metadata for a custom query root") {
      val customRoot =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"]) { query: RootQuery }
           |$directives
           |type RootQuery { expensive: String @cost(weight: 5) }
           |""".stripMargin
      for {
        remote  <- stub("""{"data":{"expensive":"value"}}""")
        runtime <- Gateway
                     .compose(Subgraph.federation("custom", remote.endpoint, customRoot))
                     .withConfig(_.withMaxOperationCost(4))
                     .interpreter
        result  <- runtime.execute("{ expensive }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("supports a standalone cost-spec alias and hides listSize from the API schema") {
      val aliased =
        s"""
           |schema @link(
           |  url: "https://specs.apollo.dev/cost/v0.1",
           |  import: [{ name: "@listSize", as: "@pageSize" }]
           |) { query: Query }
           |${directives.replace("@cost", "@ignoredCost")}
           |${listSizeDefinition.replace("@listSize", "@pageSize")}
           |type Query { books: [Book!]! @pageSize(assumedSize: 3) }
           |type Book { title: String }
           |""".stripMargin
      for {
        remote        <- stub("""{"data":{"books":[]}}""")
        limited       <- Gateway
                           .compose(Subgraph.federation("books", remote.endpoint, aliased))
                           .withConfig(_.withMaxOperationCost(2))
                           .interpreter
        rejected      <- limited.execute("{ books { title } }")
        introspection <- Gateway
                           .compose(Subgraph.federation("books", remote.endpoint, aliased))
                           .interpreter
                           .flatMap(_.execute("{ __schema { directives { name } } }"))
        directives     = field(introspection.data, "__schema")
                           .flatMap(field(_, "directives"))
                           .collect { case ResponseListValue(values) =>
                             values.flatMap(field(_, "name")).collect { case StringValue(name) => name }
                           }
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        introspection.errors.isEmpty,
        directives.exists(!_.contains("pageSize"))
      )
    },
    test("validates listSize paths and Federation version") {
      def invalid(version: String, application: String) =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/$version", import: ["@listSize"]) { query: Query }
           |$directives
           |$listSizeDefinition
           |type Query { books(first: Int): Cursor! $application }
           |type Cursor { page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        oldVersion <- Gateway
                        .compose(
                          Subgraph.federation(
                            "old",
                            unreachableEndpoint,
                            invalid("v2.8", "@listSize(slicingArguments: [\"first\"], sizedFields: [\"page\"])")
                          )
                        )
                        .interpreter
                        .exit
        badPath    <- Gateway
                        .compose(
                          Subgraph.federation(
                            "bad-path",
                            unreachableEndpoint,
                            invalid("v2.9", "@listSize(slicingArguments: [\"missing\"], sizedFields: [\"unknown\"])")
                          )
                        )
                        .interpreter
                        .exit
        badSized   <- Gateway
                        .compose(
                          Subgraph.federation(
                            "bad-sized",
                            unreachableEndpoint,
                            invalid("v2.9", "@listSize(assumedSize: 2, sizedFields: [\"unknown\"])")
                          )
                        )
                        .interpreter
                        .exit
        listInput  <- Gateway
                        .compose(
                          Subgraph.federation(
                            "list-input",
                            unreachableEndpoint,
                            invalid("v2.9", "@listSize(slicingArguments: [\"filters.first\"], sizedFields: [\"page\"])")
                              .replace(
                                "type Query { books(first: Int): Cursor!",
                                "input Filter { first: Int } type Query { books(first: Int, filters: [Filter!]): Cursor!"
                              )
                          )
                        )
                        .interpreter
                        .exit
        manyLeaves <- Gateway
                        .compose(
                          Subgraph.federation(
                            "many-leaves",
                            unreachableEndpoint,
                            invalid("v2.9", "@listSize(assumedSize: 2, sizedFields: [\"page recent\"])")
                              .replace(
                                "type Cursor { page: [Book!]! }",
                                "type Cursor { page: [Book!]! recent: [Book!]! }"
                              )
                          )
                        )
                        .interpreter
                        .exit
        mixedLinks <- Gateway
                        .compose(
                          Subgraph.federation(
                            "mixed-links",
                            unreachableEndpoint,
                            s"""
                               |schema
                               |  @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost"])
                               |  @link(url: "https://specs.apollo.dev/cost/v0.2", import: [{ name: "@cost", as: "@futureCost" }])
                               |  { query: Query }
                               |$directives
                               |${costDefinition.replace("@cost", "@futureCost")}
                               |type Query { value: String @futureCost(weight: 5) }
                               |""".stripMargin
                          )
                        )
                        .interpreter
                        .exit
        oldErrors   = buildDiagnostics(oldVersion)
        pathErrors  = buildDiagnostics(badPath)
        sizedErrors = buildDiagnostics(badSized)
        listErrors  = buildDiagnostics(listInput)
        leafErrors  = buildDiagnostics(manyLeaves)
        linkErrors  = buildDiagnostics(mixedLinks)
      } yield assertTrue(
        oldErrors.exists(_.contains("@listSize requires Federation v2.9 or cost spec v0.1")),
        pathErrors.exists(_.contains("slicing argument 'missing' must resolve to an Int or list argument")),
        sizedErrors.exists(_.contains("sized field 'unknown' must exist and return a list")),
        listErrors.exists(_.contains("slicing argument 'filters.first' must resolve to an Int or list argument")),
        leafErrors.exists(_.contains("sized field 'page recent' must not select sibling leaf fields")),
        linkErrors.exists(_.contains("@cost requires Federation v2.9 or cost spec v0.1"))
      )
    },
    test("counts injected key selections once across planned federation requests") {
      val products =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@cost"]) { query: Query }
           |$federationDirectives
           |$costDefinition
           |union _Entity = Product
           |type Query {
           |  product: Product
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Product @key(fields: "id") { id: ID! @cost(weight: 4) }
           |""".stripMargin
      val reviews  =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external", "@cost"]) { query: Query }
           |$federationDirectives
           |$costDefinition
           |union _Entity = Product
           |type Query {
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Product @key(fields: "id") { id: ID! @external expensive: String @cost(weight: 5) }
           |""".stripMargin
      for {
        productsRemote <-
          stub(
            """{"data":{"product":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product"}}}"""
          )
        reviewsRemote  <-
          stub(
            """{"data":{"_entities":[{"expensive":"yes","_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product"}]}}"""
          )
        rejected       <- Gateway
                            .compose(
                              Subgraph.federation("products", productsRemote.endpoint, products),
                              Subgraph.federation("reviews", reviewsRemote.endpoint, reviews)
                            )
                            .withConfig(_.withMaxOperationCost(10))
                            .interpreter
        rejectedResult <- rejected.execute("{ product { expensive } }")
        rejectedSent   <- productsRemote.requests.get.zip(reviewsRemote.requests.get)
        accepted       <- Gateway
                            .compose(
                              Subgraph.federation("products", productsRemote.endpoint, products),
                              Subgraph.federation("reviews", reviewsRemote.endpoint, reviews)
                            )
                            .withConfig(_.withMaxOperationCost(11))
                            .interpreter
        acceptedResult <- accepted.execute("{ product { expensive } }")
        sent           <- productsRemote.requests.get.zip(reviewsRemote.requests.get)
      } yield assertTrue(
        rejectedResult.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        rejectedSent._1.isEmpty,
        rejectedSent._2.isEmpty,
        acceptedResult.errors.isEmpty,
        sent._1.size == 1,
        sent._2.size == 1
      )
    },
    test("propagates list sizes into downstream entity fetches") {
      val products =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@listSize"]) { query: Query }
           |$federationDirectives
           |$listSizeDefinition
           |union _Entity = Product
           |type Query {
           |  products: [Product!]! @listSize(assumedSize: 3)
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Product @key(fields: "id") { id: ID! }
           |""".stripMargin
      val reviews  =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external"]) { query: Query }
           |$federationDirectives
           |union _Entity = Product
           |type Query {
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Product @key(fields: "id") { id: ID! @external reviews: Review }
           |type Review { body: String }
           |""".stripMargin
      for {
        productsRemote <- stub("{}")
        reviewsRemote  <- stub("{}")
        runtime        <- Gateway
                            .compose(
                              Subgraph.federation("products", productsRemote.endpoint, products),
                              Subgraph.federation("reviews", reviewsRemote.endpoint, reviews)
                            )
                            .withConfig(_.withMaxOperationCost(8))
                            .interpreter
        result         <- runtime.execute("{ products { reviews { body } } }")
        sent           <- productsRemote.requests.get.zip(reviewsRemote.requests.get)
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent._1.isEmpty,
        sent._2.isEmpty
      )
    },
    test("propagates sized fields into a downstream entity fetch") {
      val cursors =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external", "@listSize"]) { query: Query }
           |$federationDirectives
           |$listSizeDefinition
           |union _Entity = Cursor
           |type Query {
           |  cursor: Cursor! @listSize(assumedSize: 3, sizedFields: ["page"])
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Cursor @key(fields: "id") { id: ID! page: [Book!]! @external }
           |type Book { placeholder: String }
           |""".stripMargin
      val books   =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external"]) { query: Query }
           |$federationDirectives
           |union _Entity = Cursor
           |type Query {
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Cursor @key(fields: "id") { id: ID! @external page: [Book!]! }
           |type Book { title: String }
           |""".stripMargin
      for {
        cursorsRemote <- stub("{}")
        booksRemote   <- stub("{}")
        runtime       <- Gateway
                           .compose(
                             Subgraph.federation("cursors", cursorsRemote.endpoint, cursors),
                             Subgraph.federation("books", booksRemote.endpoint, books)
                           )
                           .withConfig(_.withMaxOperationCost(3))
                           .interpreter
        result        <- runtime.execute("{ cursor { page { title } } }")
        sent          <- cursorsRemote.requests.get.zip(booksRemote.requests.get)
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent._1.isEmpty,
        sent._2.isEmpty
      )
    },
    test("takes the maximum of mutually exclusive concrete entity fetches") {
      val nodes    =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@cost"]) { query: Query }
           |$federationDirectives
           |$costDefinition
           |union _Entity = Product | User
           |type Query {
           |  node: Node
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |interface Node { id: ID! }
           |type Product implements Node @key(fields: "id") { id: ID! }
           |type User implements Node @key(fields: "id") { id: ID! }
           |""".stripMargin
      val products =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external", "@cost"]) { query: Query }
           |$federationDirectives
           |$costDefinition
           |union _Entity = Product
           |type Query {
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type Product @key(fields: "id") { id: ID! @external productInfo: String @cost(weight: 5) }
           |""".stripMargin
      val users    =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@key", "@external", "@cost"]) { query: Query }
           |$federationDirectives
           |$costDefinition
           |union _Entity = User
           |type Query {
           |  _entities(representations: [_Any!]!): [_Entity]!
           |  _service: _Service!
           |}
           |type User @key(fields: "id") { id: ID! @external userInfo: String @cost(weight: 7) }
           |""".stripMargin
      val query    = "{ node { ... on Product { productInfo } ... on User { userInfo } } }"
      for {
        nodesRemote     <-
          stub(
            """{"data":{"node":{"_caliban_gateway_key":"p1","_caliban_gateway_typename":"Product","_caliban_gateway_key_2":"p1","_caliban_gateway_typename_2":"Product","_caliban_gateway_key_3":"p1","_caliban_gateway_typename_3":"Product","_caliban_gateway_key_4":"p1","_caliban_gateway_typename_4":"Product","_caliban_gateway_runtime_typename":"Product"}}}"""
          )
        productsRemote  <-
          stub(
            """{"data":{"_entities":[{"productInfo":"details","_caliban_gateway_entity_key":"p1","_caliban_gateway_entity_typename":"Product"}]}}"""
          )
        usersRemote     <- stub("""{"data":{"_entities":[]}}""")
        gateway          = Gateway.compose(
                             Subgraph.federation("nodes", nodesRemote.endpoint, nodes),
                             Subgraph.federation("products", productsRemote.endpoint, products),
                             Subgraph.federation("users", usersRemote.endpoint, users)
                           )
        rejectedRuntime <- gateway.withConfig(_.withMaxOperationCost(8)).interpreter
        rejected        <- rejectedRuntime.execute(query)
        acceptedRuntime <- gateway.withConfig(_.withMaxOperationCost(9)).interpreter
        accepted        <- acceptedRuntime.execute(query)
        sent            <- nodesRemote.requests.get.zip(productsRemote.requests.get).zip(usersRemote.requests.get)
      } yield assertTrue(
        rejected.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        accepted.errors.isEmpty,
        sent._1.size == 1,
        sent._2.size == 1,
        sent._3.isEmpty
      )
    },
    test("uses the maximum cost declared for a shared field") {
      def shared(weight: Int) =
        s"""
           |schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@cost", "@shareable"]) { query: Query }
           |$directives
           |type Query { value: String @shareable @cost(weight: $weight) }
           |""".stripMargin
      for {
        first   <- stub("""{"data":{"value":"first"}}""")
        second  <- stub("""{"data":{"value":"second"}}""")
        runtime <- Gateway
                     .compose(
                       Subgraph.federation("first", first.endpoint, shared(5)),
                       Subgraph.federation("second", second.endpoint, shared(10))
                     )
                     .withConfig(_.withMaxOperationCost(9))
                     .interpreter
        result  <- runtime.execute("{ value }")
        sent    <- first.requests.get.zip(second.requests.get)
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent._1.isEmpty,
        sent._2.isEmpty
      )
    },
    test("adds the mutation base cost") {
      val mutationSchema =
        """
          |schema { query: Query mutation: Mutation }
          |type Query { value: String }
          |type Mutation { update: String }
          |""".stripMargin
      for {
        remote  <- stub("""{"data":{"update":"ok"}}""")
        runtime <- Gateway
                     .compose(Subgraph.graphql("mutation", remote.endpoint, mutationSchema))
                     .withConfig(_.withMaxOperationCost(9))
                     .interpreter
        result  <- runtime.execute("mutation { update }")
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.flatMap(code) == List("COST_ESTIMATED_TOO_EXPENSIVE"),
        sent.isEmpty
      )
    },
    test("rejects a non-positive operation cost limit at build time") {
      for {
        exit <- Gateway
                  .compose(Subgraph.local("local", localValueGraph(ZIO.succeed("ok"))))
                  .withConfig(_.withMaxOperationCost(0))
                  .interpreter
                  .exit
      } yield assertTrue(
        buildDiagnostics(exit) == List("Gateway maxOperationCost must be positive.")
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
