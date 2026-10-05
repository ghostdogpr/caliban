package caliban.gateway

import caliban.gateway.CompositionDiagnostic.Code
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.SchemaCoordinate.{ Argument, Member }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.composition.ComposedGraph.LookupOperation
import caliban.interop.jsoniter.ValueJsoniter.responseValueCodec
import caliban.schema.Annotations.{ GQLInterface, GQLName, GQLOneOfInput }
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, GraphQL, ResponseValue, RootResolver }
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import zio._
import zio.http._
import zio.test._

object CompositeLookupSpec extends ZIOSpecDefault {

  private def lookups(graph: ComposedGraph, subgraph: String, typeName: String): List[ComposedGraph.EntityLookup] =
    graph.sources.filter(_.name == subgraph).flatMap(_.entityLookups(typeName))

  private def lookupTypes(graph: ComposedGraph, subgraph: String): List[String] =
    List("Book", "Clothing", "Electronics", "Movie", "Person", "Podcast", "Product").filter(
      lookups(graph, subgraph, _).nonEmpty
    )

  private def served(api: GraphQL[Any]): ZIO[Server with Ref[Int], Nothing, Stub] =
    api.interpreter.orDie.flatMap(interpreter =>
      stubByRequestZIO(request =>
        interpreter.executeRequest(request).map(response => writeToString[ResponseValue](response.toResponseValue))
      )
    )

  private def queries(stub: Stub): UIO[List[String]] = stub.requests.get.map(_.toList.flatMap(_.query))

  private object People extends GenericSchema[Any] {
    import auto._

    final case class Address(id: String)
    final case class Tag(name: String)
    final case class Person(id: String, name: String, address: Address, tags: List[Tag])
    final case class Query(people: List[Person])

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          List(
            Person("p1", "Ada", Address("a1"), List(Tag("math"))),
            Person("p2", "Bob", Address("a2"), List(Tag("art")))
          )
        )
      )
    )

    val sdl =
      """type Query { people: [Person!]! }
        |type Person @key(fields: "id") {
        |  id: ID!
        |  name: String! @shareable
        |  address: Address! @shareable
        |  tags: [Tag!]!
        |}
        |type Address @shareable { id: ID! }
        |type Tag { name: String! }""".stripMargin
  }

  private object Residents extends GenericSchema[Any] {
    import auto._

    final case class Address(id: String)
    @GQLName("Person")
    final case class Resident(name: String, address: Address)
    final case class Query(residents: List[Resident])

    val api: GraphQL[Any] = graphQL(RootResolver(Query(List(Resident("Bob", Address("a2"))))))

    val sdl =
      """type Query { residents: [Person!]! }
        |type Person { name: String! @shareable address: Address! @shareable }
        |type Address @shareable { id: ID! }""".stripMargin
  }

  // Every lookup shape over Person; each test's SDL declares only the one it exercises.
  private object Bios extends GenericSchema[Any] {
    import auto._

    private val people = List(("p1", "Ada", "a1", "math"), ("p2", "Bob", "a2", "art"))

    @GQLName("Person")
    final case class Bio(bio: String)
    final case class ById(id: String)
    final case class ByName(name: String)
    final case class ByTags(tags: List[String])
    @GQLOneOfInput
    @GQLName("PersonBy")
    sealed trait PersonBy
    object PersonBy {
      final case class Id(id: String)               extends PersonBy
      final case class AddressId(addressId: String) extends PersonBy
    }
    final case class By(by: PersonBy)
    final case class Lookups(personById: ById => Option[Bio])
    final case class Query(
      personByAddressId: ById => Option[Bio],
      person: By => Option[Bio],
      lookups: Lookups,
      personById: ById => Option[Bio],
      personByName: ByName => Option[Bio],
      personByTags: ByTags => Option[Bio]
    )

    implicit val byIdArg: ArgBuilder[ById]                  = ArgBuilder.gen
    implicit val byNameArg: ArgBuilder[ByName]              = ArgBuilder.gen
    implicit val byTagsArg: ArgBuilder[ByTags]              = ArgBuilder.gen
    implicit val idArg: ArgBuilder[PersonBy.Id]             = ArgBuilder.gen
    implicit val addressArg: ArgBuilder[PersonBy.AddressId] = ArgBuilder.gen
    implicit val personByArg: ArgBuilder[PersonBy]          = ArgBuilder.gen
    implicit val byArg: ArgBuilder[By]                      = ArgBuilder.gen
    implicit val personBySchema: Schema[Any, PersonBy]      = Schema.gen

    private def find(matches: ((String, String, String, String)) => Boolean): Option[Bio] =
      people.find(matches).map(person => Bio(s"${person._2}'s bio"))

    private val byId: ById => Option[Bio] = args => find(_._1 == args.id)

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          args => find(_._3 == args.id),
          args =>
            args.by match {
              case PersonBy.Id(id)               => find(_._1 == id)
              case PersonBy.AddressId(addressId) => find(_._3 == addressId)
            },
          Lookups(byId),
          byId,
          args => find(_._2 == args.name),
          args => find(person => args.tags == List(person._4))
        )
      )
    )
  }

  private object Catalog extends GenericSchema[Any] {
    import auto._

    @GQLInterface
    sealed trait Media
    object Media {
      final case class Book(title: String, isbn: String)       extends Media
      final case class Movie(title: String, upc: String)       extends Media
      final case class Podcast(title: String, feedUrl: String) extends Media
    }
    final case class Review(id: String, subject: Media)
    final case class Query(media: List[Media], reviews: List[Review])

    private val book  = Media.Book("Dune", "isbn-1")
    private val movie = Media.Movie("Alien", "upc-1")

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(List(book, movie, Media.Podcast("Radiolab", "feed-1")), List(Review("r1", book), Review("r2", movie)))
      )
    )

    val sdl =
      """type Query { media: [Media!]! reviews: [Review!]! }
        |interface Media { title: String! }
        |type Book implements Media { title: String! isbn: String! }
        |type Movie implements Media { title: String! upc: String! }
        |type Podcast implements Media { title: String! feedUrl: String! }
        |type Review @key(fields: "id") { id: ID! subject: Media! }""".stripMargin
  }

  private object Ratings extends GenericSchema[Any] {
    import auto._

    @GQLInterface
    sealed trait Media
    object Media    {
      final case class Book(rating: Int)    extends Media
      final case class Movie(rating: Int)   extends Media
      final case class Podcast(rating: Int) extends Media
    }
    @GQLOneOfInput
    @GQLName("MediaKeyInput")
    sealed trait MediaKey
    object MediaKey {
      final case class Isbn(isbn: String)       extends MediaKey
      final case class Upc(upc: String)         extends MediaKey
      final case class FeedUrl(feedUrl: String) extends MediaKey
    }
    final case class ByKey(key: MediaKey)
    final case class ByIsbn(isbn: String)
    final case class Review(score: Option[Int])
    final case class Query(mediaByKey: ByKey => Option[Media], reviewBySubjectIsbn: ByIsbn => Option[Review])

    implicit val isbnArg: ArgBuilder[MediaKey.Isbn]    = ArgBuilder.gen
    implicit val upcArg: ArgBuilder[MediaKey.Upc]      = ArgBuilder.gen
    implicit val feedArg: ArgBuilder[MediaKey.FeedUrl] = ArgBuilder.gen
    implicit val keyArg: ArgBuilder[MediaKey]          = ArgBuilder.gen
    implicit val byKeyArg: ArgBuilder[ByKey]           = ArgBuilder.gen
    implicit val byIsbnArg: ArgBuilder[ByIsbn]         = ArgBuilder.gen
    implicit val mediaKeySchema: Schema[Any, MediaKey] = Schema.gen

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          args =>
            Some(args.key match {
              case MediaKey.Isbn(_)    => Media.Book(5)
              case MediaKey.Upc(_)     => Media.Movie(4)
              case MediaKey.FeedUrl(_) => Media.Podcast(3)
            }),
          args => Some(Review(Some(args.isbn.length)))
        )
      )
    )

    val mediaSdl =
      """type Query {
        |  mediaByKey(
        |    key: MediaKeyInput! @is(field: "{ isbn: <Book>.isbn } | { upc: <Movie>.upc } | { feedUrl: <Podcast>.feedUrl }")
        |  ): Media @lookup
        |}
        |input MediaKeyInput @oneOf { isbn: String upc: String feedUrl: String }
        |interface Media { rating: Int! }
        |type Book implements Media { rating: Int! }
        |type Movie implements Media { rating: Int! }
        |type Podcast implements Media { rating: Int! }""".stripMargin

    val reviewSdl =
      """type Query { reviewBySubjectIsbn(isbn: String! @is(field: "subject<Book>.isbn")): Review @lookup @internal }
        |type Review { score: Int }""".stripMargin
  }

  private def bios(lookups: String, extra: String = ""): String =
    s"type Query { $lookups } type Person { bio: String! } $extra"

  private final case class PeopleGateway(runtime: GatewayInterpreter[Any], bios: Stub)

  private def peopleGateway(biosSdl: String): ZIO[Server with Ref[Int] with Scope, Nothing, PeopleGateway] =
    for {
      people    <- served(People.api)
      residents <- served(Residents.api)
      bioStub   <- served(Bios.api)
      runtime   <- Gateway
                     .compose(
                       Subgraph.graphql("people", people.endpoint, People.sdl),
                       Subgraph.graphql("residents", residents.endpoint, Residents.sdl),
                       Subgraph.graphql("bios", bioStub.endpoint, biosSdl)
                     )
                     .interpreter
                     .orDie
    } yield PeopleGateway(runtime, bioStub)

  private val peopleBios =
    """{"people":[{"name":"Ada","bio":"Ada's bio"},{"name":"Bob","bio":"Bob's bio"}]}"""

  def spec = suite("CompositeLookupSpec")(
    suite("composition of the spec's examples")(
      test("several lookups for one entity") {
        val sdl = """type Query {
                    |  version: Int
                    |  productById(id: ID!): Product @lookup
                    |  productByName(name: String!): Product @lookup
                    |}
                    |type Product { id: ID! name: String! }""".stripMargin
        assertTrue(composeComposite("a" -> sdl).map(lookups(_, "a", "Product").size) == Right(2))
      },
      test("a lookup returning a union resolves each possible type, covered or not") {
        def sdl(clothing: String) =
          s"""type Query { product(id: ID!, categoryId: Int): Product @lookup }
             |union Product = Electronics | Clothing
             |type Electronics { id: ID! categoryId: Int name: String brand: String price: Float }
             |type Clothing { $clothing }""".stripMargin
        val covered               = composeComposite("a" -> sdl("id: ID! categoryId: Int name: String size: String price: Float"))
        val uncovered             = composeComposite("a" -> sdl("id: ID! name: String size: String price: Float"))
        assertTrue(
          covered.map(lookupTypes(_, "a")) == Right(List("Clothing", "Electronics")),
          uncovered.isRight
        )
      },
      test("lookups nested under argument-less fields, internal ones included") {
        val nested   = """type Query { lookups: Lookups! }
                       |type Lookups { productById(id: ID!): Product @lookup }
                       |type Product { id: ID! }""".stripMargin
        val internal = """type Query { productById(id: ID!): Product @lookup lookups: InternalLookups! @internal }
                         |type InternalLookups @internal { productBySku(sku: ID!): Product @lookup }
                         |type Product @key(fields: "id") @key(fields: "sku") { id: ID! sku: ID! }""".stripMargin
        val paths    = (graph: ComposedGraph, subgraph: String) =>
          lookups(graph, subgraph, "Product").map(_.operation).collect {
            case LookupOperation.Single(path, field, _, _) =>
              (path :+ field).mkString(".")
          }
        val graph    = composeComposite("nested" -> nested)
        val hidden   = composeComposite("internal" -> internal)
        val shortest = composeComposite(
          "shortest" -> nested.replace("type Query {", "type A { lookups: Lookups! } type Query { a: A")
        )
        val linked   =
          (0 until 12).map(i => s"type T$i { lookups: Lookups! ${(0 until 12).map(j => s"t$j: T$j").mkString(" ")} }")
        val dense    = composeComposite(
          "dense" -> nested.replace(
            "type Query { lookups: Lookups! }",
            linked.mkString("type Query { t0: T0 } ", " ", "")
          )
        )
        assertTrue(
          graph.map(paths(_, "nested")) == Right(List("lookups.productById")),
          shortest.map(paths(_, "shortest")) == Right(List("lookups.productById")),
          dense.map(paths(_, "dense")) == Right(List("t0.lookups.productById")),
          hidden.map(paths(_, "internal").toSet) == Right(Set("productById", "lookups.productBySku")),
          hidden.exists(_.rootType.queryType.allFields.map(_.name) == List("productById"))
        )
      },
      test("@is maps arguments to paths, input objects and oneOf alternatives") {
        // The spec's examples of paths and @oneOf omit @lookup, which Is Invalid Usage requires.
        val sdl  =
          """type Query {
            |  personById(productId: ID! @is(field: "id")): Person @lookup
            |  personByAddressId(id: ID! @is(field: "address.id"), kind: PersonKind @is(field: "kind")): Person @lookup
            |  person(by: PersonByInput @is(field: "{ id } | { addressId: address.id } | { name }")): Person @lookup
            |}
            |enum PersonKind { HUMAN }
            |input PersonByInput @oneOf { id: ID addressId: ID name: String }
            |type Person { id: ID! name: String! kind: PersonKind address: Address! }
            |type Address { id: ID! }""".stripMargin
        val keys = composeComposite("a" -> sdl).map(lookups(_, "a", "Person").map(_.key.map(_.name)))
        assertTrue(
          keys == Right(List(List("id"), List("address", "kind"), List("id"), List("address"), List("name")))
        )
      },
      test("@is type conditions select the possible types each alternative resolves") {
        def sdl(alternatives: String) =
          s"""type Query { mediaByKey(key: MediaKeyInput! @is(field: "$alternatives")): Media @lookup }
             |input MediaKeyInput @oneOf { isbn: String upc: String feedUrl: String }
             |interface Media { id: ID! }
             |type Book implements Media { id: ID! isbn: String! }
             |type Movie implements Media { id: ID! upc: String! }
             |type Podcast implements Media { id: ID! feedUrl: String! }""".stripMargin
        val all                       =
          composeComposite("a" -> sdl("{ isbn: <Book>.isbn } | { upc: <Movie>.upc } | { feedUrl: <Podcast>.feedUrl }"))
        val partial                   = composeComposite("a" -> sdl("{ isbn: <Book>.isbn } | { upc: <Movie>.upc }"))
        assertTrue(
          all.map(lookupTypes(_, "a")) == Right(List("Book", "Movie", "Podcast")),
          all.map(lookups(_, "a", "Book").map(_.key.map(_.name))) == Right(List(List("isbn"))),
          partial.map(lookupTypes(_, "a")) == Right(List("Book", "Movie"))
        )
      },
      test("@is may select fields that only other subgraphs define") {
        val a = """type Query { personByName(name: String! @is(field: "name")): Person @lookup }
                  |type Person { id: ID! }""".stripMargin
        val b = """type Query { people: [Person] } type Person @key(fields: "id") { id: ID! name: String! }"""
        assertTrue(
          composeComposite("a" -> a.replace("type Person", "type Person @key(fields: \"id\")"), "b" -> b).isRight
        )
      },
      test("@is maps are validated in client names under renamed types and arguments") {
        def product(query: String, transformation: SchemaTransformation) =
          composeSdl(
            List("a" -> s"type Query { $query } enum Sku { A } type Product { id: ID! sku: Sku! }"),
            federation = false,
            Map("a"  -> List(transformation))
          )
        val renamedType                                                  =
          product(
            """product(sku: Sku! @is(field: "sku")): Product @lookup""",
            SchemaTransformation.renameType("Sku", "Code")
          )
        val renamedArgument                                              = product(
          """product(id: ID! @is(field: "unknownField")): Product @lookup""",
          SchemaTransformation.renameArgument("Query", "product", "id", "key")
        )
        assertTrue(
          renamedType.map(lookups(_, "a", "Product").size) == Right(1),
          renamedArgument.reports(Code.IsInvalidFields, Argument("Query", "product", "key"), "a")
        )
      }
    ),
    suite("rejections")(
      test("reports the spec's rules for @is and @lookup") {
        def product(query: String) =
          composeComposite("a" -> s"type Query { $query } type Product { id(scope: Int): ID! name: String }")
        val argument               = Argument("Query", "product", "id")
        val field                  = Member("Query", "product")
        assertTrue(
          product("""product(id: ID! @is(field: "{ id ")): Product @lookup""")
            .reports(Code.IsInvalidSyntax, argument, "a"),
          product("""product(id: ID! @is(field: "id(scope: 1)")): Product @lookup""")
            .reports(Code.IsFieldsHasArguments, argument, "a"),
          product("""product(id: ID! @is(field: "id")): Product""").reports(Code.IsInvalidUsage, argument, "a"),
          product("""product(id: ID! @is(field: "unknownField")): Product @lookup""")
            .reports(Code.IsInvalidFields, argument, "a"),
          product("""product(id: Int! @is(field: "id")): Product @lookup""")
            .reports(Code.IsInvalidFields, argument, "a"),
          product("product(id: ID!): [Product] @lookup").reports(Code.LookupReturnsList, field, "a"),
          product("product: Product @lookup").reports(Code.LookupMustHaveArguments, field, "a")
        )
      },
      test("checks @lookup fields that no lookup path reaches") {
        val sdl =
          """type Query { catalog(region: String): Catalog }
            |type Catalog {
            |  products(id: ID!): [Product] @lookup
            |  product(id: ID! @is(field: "{ id ")): Product @lookup
            |}
            |type Product { id: ID! }""".stripMargin
        assertTrue(
          composeComposite("a" -> sdl).reports(Code.LookupReturnsList, Member("Catalog", "products"), "a"),
          composeComposite("a" -> sdl).reports(Code.IsInvalidSyntax, Argument("Catalog", "product", "id"), "a")
        )
      }
    ),
    suite("resolution")(
      test("through an @is path to a key another subgraph defines") {
        for {
          gateway <- peopleGateway(bios("""personByAddressId(id: ID! @is(field: "address.id")): Person @lookup"""))
          result  <- gateway.runtime.execute("{ people { name bio } }")
          sent    <- queries(gateway.bios)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == peopleBios,
          sent.exists(_.contains("""personByAddressId(id:"a1")"""))
        )
      },
      test("through several lookups, by the key each source can supply") {
        val sdl = bios("personById(id: ID!): Person @lookup personByName(name: String!): Person @lookup")
        for {
          gateway   <- peopleGateway(sdl)
          people    <- gateway.runtime.execute("{ people { bio } }")
          residents <- gateway.runtime.execute("{ residents { bio } }")
          sent      <- queries(gateway.bios)
        } yield assertTrue(
          people.errors.isEmpty,
          residents.data.toString == """{"residents":[{"bio":"Bob's bio"}]}""",
          sent.exists(_.contains("""personById(id:"p1")""")),
          sent.exists(_.contains("""personByName(name:"Bob")"""))
        )
      },
      test("through @oneOf alternatives, by the key each source can supply") {
        val sdl = bios(
          """person(by: PersonBy! @is(field: "{ id } | { addressId: address.id }")): Person @lookup""",
          "input PersonBy @oneOf { id: ID addressId: ID }"
        )
        for {
          gateway   <- peopleGateway(sdl)
          people    <- gateway.runtime.execute("{ people { name bio } }")
          residents <- gateway.runtime.execute("{ residents { bio } }")
          sent      <- queries(gateway.bios)
        } yield assertTrue(
          people.data.toString == peopleBios,
          residents.data.toString == """{"residents":[{"bio":"Bob's bio"}]}""",
          sent.exists(_.contains("""person(by:{id:"p1"})""")),
          sent.exists(_.contains("""person(by:{addressId:"a2"})"""))
        )
      },
      test("through a lookup nested under an internal argument-less field") {
        val sdl = bios(
          "lookups: Lookups! @internal",
          "type Lookups @internal { personById(id: ID!): Person @lookup }"
        )
        for {
          gateway <- peopleGateway(sdl)
          result  <- gateway.runtime.execute("{ people { name bio } }")
          sent    <- queries(gateway.bios)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString == peopleBios,
          sent.exists(_.contains("""lookups{_caliban_gateway_lookup_0:personById(id:"p1")"""))
        )
      },
      test("through a list selection") {
        for {
          gateway <- peopleGateway(bios("""personByTags(tags: [String!]! @is(field: "tags[name]")): Person @lookup"""))
          result  <- gateway.runtime.execute("{ people { name bio } }")
        } yield assertTrue(result.errors.isEmpty, result.data.toString == peopleBios)
      },
      test("through a lookup returning an interface, by type-conditioned alternatives") {
        for {
          catalog <- served(Catalog.api)
          ratings <- served(Ratings.api)
          runtime <- Gateway
                       .compose(
                         Subgraph.graphql("catalog", catalog.endpoint, Catalog.sdl),
                         Subgraph.graphql("ratings", ratings.endpoint, Ratings.mediaSdl)
                       )
                       .interpreter
          result  <- runtime.execute("{ media { title rating } }")
          sent    <- queries(ratings)
        } yield assertTrue(
          result.errors.isEmpty,
          result.data.toString ==
            """{"media":[{"title":"Dune","rating":5},{"title":"Alien","rating":4},{"title":"Radiolab","rating":3}]}""",
          sent.contains(
            """query __GatewayLookup{_caliban_gateway_lookup_0:mediaByKey(key:{isbn:"isbn-1"}){...on Book{rating}}}"""
          )
        )
      },
      test("through a type condition below the root, for the entities it applies to") {
        for {
          catalog <- served(Catalog.api)
          ratings <- served(Ratings.api)
          runtime <- Gateway
                       .compose(
                         Subgraph.graphql("catalog", catalog.endpoint, Catalog.sdl),
                         Subgraph.graphql("ratings", ratings.endpoint, Ratings.reviewSdl)
                       )
                       .interpreter
          result  <- runtime.execute("{ reviews { id score } }")
          sent    <- queries(ratings)
        } yield assertTrue(
          result.data.toString == """{"reviews":[{"id":"r1","score":6},{"id":"r2","score":null}]}""",
          result.errors.size == 1,
          sent.size == 1,
          sent.exists(_.contains("""_caliban_gateway_lookup_0:reviewBySubjectIsbn(isbn:"isbn-1")""")),
          !sent.exists(_.contains("upc"))
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
}
