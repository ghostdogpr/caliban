package caliban.relay

import caliban._
import zio.test.Assertion._
import zio.test._
import zio.ZIO

object ConnectionSpec extends DefaultRunnableSpec {
  case class ItemEdge(cursor: Base64Cursor, node: Item) extends Edge[Base64Cursor, Item]

  object ItemEdge {
    def apply(x: Item, i: Int): ItemEdge = ItemEdge(Base64Cursor(i), x)
  }

  case class ItemConnection(
    pageInfo: PageInfo,
    edges: List[ItemEdge]
  ) extends Connection[ItemEdge]

  object ItemConnection {
    val fromList =
      Connection.fromList(ItemConnection.apply)(ItemEdge.apply)(_, _)
  }

  case class Item(name: String)

  case class Args(
    first: Option[Int],
    last: Option[Int],
    before: Option[String],
    after: Option[String]
  ) extends PaginationArgs[Base64Cursor]

  case class ForwardArgs(
    first: Option[Int],
    after: Option[String]
  ) extends ForwardPaginationArgs[Base64Cursor]

  case class BackwardArgs(
    last: Option[Int],
    before: Option[String]
  ) extends BackwardPaginationArgs[Base64Cursor]

  val conn = ItemConnection.fromList(
    List(Item("a"), Item("b"), Item("c")),
    Pagination(
      cursor = PaginationCursor.NoCursor,
      count = PaginationCount.First(2)
    )
  )

  case class Query(connection: Args => ZIO[Any, CalibanError, ItemConnection])
  val api = GraphQL.graphQL(
    RootResolver(
      Query(args =>
        for {
          pageInfo <- Pagination(args)
          items     = ItemConnection.fromList(List(Item("1"), Item("2"), Item("3")), pageInfo)
        } yield items
      )
    )
  )

  def spec = suite("ConnectionSpec")(
    test("it correctly creates the connection") {
      assertTrue(
        conn ==
          ItemConnection(
            PageInfo(
              hasNextPage = true,
              hasPreviousPage = false,
              startCursor = Some("Y3Vyc29yOjA="),
              endCursor = Some("Y3Vyc29yOjE=")
            ),
            List(
              ItemEdge(Base64Cursor(0), Item("a")),
              ItemEdge(Base64Cursor(1), Item("b"))
            )
          )
      )
    },
    test("it correctly slices the list when before is set") {
      assertTrue(
        ItemConnection
          .fromList(
            List(Item("a"), Item("b"), Item("c")),
            Pagination(cursor = PaginationCursor.Before(Base64Cursor(2)), count = PaginationCount.First(3))
          )
          .edges == List(
          ItemEdge(Base64Cursor(0), Item("a")),
          ItemEdge(Base64Cursor(1), Item("b"))
        )
      )
    },
    test("it correctly calculates hasNextPage") {
      val list = List(Item("a"), Item("b"), Item("c"))

      val calcHasNextPage = (cursor: PaginationCursor[Base64Cursor], count: PaginationCount) =>
        ItemConnection
          .fromList(
            list,
            Pagination(cursor = cursor, count = count)
          )
          .pageInfo
          .hasNextPage

      // when first is set
      assert(calcHasNextPage(PaginationCursor.NoCursor, PaginationCount.First(2)))(isTrue) &&
      assert(calcHasNextPage(PaginationCursor.NoCursor, PaginationCount.First(3)))(isFalse) &&
      assert(calcHasNextPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.First(1)))(isTrue) &&
      assert(calcHasNextPage(PaginationCursor.After(Base64Cursor(1)), PaginationCount.First(1)))(isFalse) &&
      // when before is set
      assert(calcHasNextPage(PaginationCursor.Before(Base64Cursor(2)), PaginationCount.First(1)))(isTrue) &&
      assert(calcHasNextPage(PaginationCursor.Before(Base64Cursor(2)), PaginationCount.First(2)))(isFalse) &&
      // when last is set, the result is always false
      assert(calcHasNextPage(PaginationCursor.NoCursor, PaginationCount.Last(3)))(isFalse) &&
      assert(calcHasNextPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.Last(3)))(isFalse) &&
      assert(calcHasNextPage(PaginationCursor.Before(Base64Cursor(1)), PaginationCount.Last(3)))(isFalse)
    },
    test("it correctly calculates hasPreviousPage") {
      val list = List(Item("a"), Item("b"), Item("c"))

      val calcHasPreviousPage = (cursor: PaginationCursor[Base64Cursor], count: PaginationCount) =>
        ItemConnection
          .fromList(
            list,
            Pagination(cursor = cursor, count = count)
          )
          .pageInfo
          .hasPreviousPage

      // when last is set
      assert(calcHasPreviousPage(PaginationCursor.NoCursor, PaginationCount.Last(2)))(isTrue) &&
      assert(calcHasPreviousPage(PaginationCursor.NoCursor, PaginationCount.Last(3)))(isFalse) &&
      assert(calcHasPreviousPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.Last(1)))(isTrue) &&
      assert(calcHasPreviousPage(PaginationCursor.After(Base64Cursor(1)), PaginationCount.Last(1)))(isFalse) &&
      // when after is set
      assert(calcHasPreviousPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.Last(1)))(isTrue) &&
      assert(calcHasPreviousPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.Last(2)))(isFalse) &&
      // when first is set, the result is always false
      assert(calcHasPreviousPage(PaginationCursor.NoCursor, PaginationCount.First(3)))(isFalse) &&
      assert(calcHasPreviousPage(PaginationCursor.After(Base64Cursor(0)), PaginationCount.First(3)))(isFalse) &&
      assert(calcHasPreviousPage(PaginationCursor.Before(Base64Cursor(1)), PaginationCount.First(3)))(isFalse)
    },
    test("it paginates the response forwards") {
      for {
        int <- api.interpreter
        res <- int.execute("""{connection(first:2) { edges { node { name } } } }""")
      } yield assertTrue(
        res.data.toString == """{"connection":{"edges":[{"node":{"name":"1"}},{"node":{"name":"2"}}]}}"""
      )
    },
    test("it paginates the response forwards with a cursor") {
      for {
        int <- api.interpreter
        res <- int.execute("""{connection(first:2, after:"Y3Vyc29yOjE=") { edges { node { name } } } }""")
      } yield assertTrue(res.data.toString == """{"connection":{"edges":[{"node":{"name":"3"}}]}}""")
    },
    test("it paginates the response backwards") {
      for {
        int <- api.interpreter
        res <- int.execute("""{connection(last:2) { edges { node { name } } } }""")
      } yield assertTrue(
        res.data.toString == """{"connection":{"edges":[{"node":{"name":"2"}},{"node":{"name":"3"}}]}}"""
      )
    },
    test("it paginates the response backwards with a cursor") {
      for {
        int <- api.interpreter
        res <- int.execute("""{connection(last:2, before: "Y3Vyc29yOjE=") { edges { node { name } } } }""")
      } yield assertTrue(res.data.toString == """{"connection":{"edges":[{"node":{"name":"1"}}]}}""")
    },
    test("it correctly renders as GraphQL") {
      val expected = """schema {
                       |  query: Query
                       |}
                       |
                       |type Item {
                       |  name: String!
                       |}
                       |
                       |type ItemConnection {
                       |  pageInfo: PageInfo!
                       |  edges: [ItemEdge!]!
                       |}
                       |
                       |type ItemEdge {
                       |  cursor: String!
                       |  node: Item!
                       |}
                       |
                       |type PageInfo {
                       |  hasNextPage: Boolean!
                       |  hasPreviousPage: Boolean!
                       |  startCursor: String
                       |  endCursor: String
                       |}
                       |
                       |type Query {
                       |  connection(first: Int, last: Int, before: String, after: String): ItemConnection
                       |}""".stripMargin

      assertTrue(api.render == expected)
    },
    suite("Pagination")(
      test("successfully returns a Pagination case class") {
        val res = Args(
          first = Some(1),
          last = None,
          after = Some(Cursor[Base64Cursor].encode(Base64Cursor(1))),
          before = None
        ).toPagination

        assertM(res)(
          equalTo(
            Pagination(
              count = PaginationCount.First(1),
              cursor = PaginationCursor.After(Base64Cursor(1))
            )
          )
        )
      },
      test("cursor can be null") {
        val res = Args(
          first = Some(1),
          last = None,
          after = None,
          before = None
        ).toPagination

        assertM(res)(
          equalTo(
            Pagination(
              count = PaginationCount.First(1),
              cursor = PaginationCursor.NoCursor
            )
          )
        )
      },
      test("both cursors and counts can't be set") {
        val res = Args(
          first = Some(1),
          last = Some(1),
          after = Some("dummy"),
          before = Some("dummy")
        ).toPagination.exit

        assertM(res)(
          fails(
            hasMessage(
              containsString("first and last cannot both be set") &&
                containsString("before and after cannot both be set")
            )
          )
        )
      },
      test("must set first or last") {
        val res = Pagination[Base64Cursor](
          first = None,
          last = None,
          after = None,
          before = None
        ).exit

        assertM(res)(
          fails(
            hasMessage(equalTo("first and last cannot both be empty"))
          )
        )
      }
    ),
    suite("ForwardPagination")(
      test("successfully returns a Pagination case class") {
        val res = ForwardArgs(
          first = Some(1),
          after = Some(Cursor[Base64Cursor].encode(Base64Cursor(1)))
        ).toPagination

        assertM(res)(
          equalTo(
            Pagination(
              count = PaginationCount.First(1),
              cursor = PaginationCursor.After(Base64Cursor(1))
            )
          )
        )
      },
      test("must set first") {
        val res = ForwardArgs(
          first = None,
          after = None
        ).toPagination.exit

        assertM(res)(
          fails(
            hasMessage(equalTo("first cannot be empty"))
          )
        )
      },
      test("first cannot be negative") {
        val res = ForwardArgs(
          first = Some(-1),
          after = None
        ).toPagination.exit

        assertM(res)(
          fails(
            hasMessage(equalTo("first cannot be negative"))
          )
        )
      }
    ),
    suite("BackwardPagination")(
      test("successfully returns a Pagination case class") {
        val res = BackwardArgs(
          last = Some(1),
          before = Some(Cursor[Base64Cursor].encode(Base64Cursor(1)))
        ).toPagination

        assertM(res)(
          equalTo(
            Pagination(
              count = PaginationCount.Last(1),
              cursor = PaginationCursor.Before(Base64Cursor(1))
            )
          )
        )
      },
      test("must set last") {
        val res = BackwardArgs(
          last = None,
          before = None
        ).toPagination.exit

        assertM(res)(
          fails(
            hasMessage(equalTo("last cannot be empty"))
          )
        )
      },
      test("last cannot be negative") {
        val res = BackwardArgs(
          last = Some(-1),
          before = None
        ).toPagination.exit

        assertM(res)(
          fails(
            hasMessage(equalTo("last cannot be negative"))
          )
        )
      }
    )
  )
}
