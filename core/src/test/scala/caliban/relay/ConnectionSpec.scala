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
    )
  )
}
