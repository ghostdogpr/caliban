# Relay Connections

The *GraphQL Cursors Connection Specification* is an additional spec that extends GraphQL to support paginating over collections in a standardized way, defined by facebook's [Relay GraphQL client](https://relay.dev/).


The spec defines a couple of types:

* Connections - the paginated 1:N relationship itself
* PageInfo - an object describing the pagination information of the current relation
* Edge - a type describing each item in the pagination
* Node - the type that's being paginated over

An example query for a connection field looks something like this:

```graphql
{
  queryName(first: 5, after: "cursor") {
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
    edges {
      cursor
      node {
        id # additional fields
      }
    }
  }
}
```

The field can be paginated forwards by using `first` (number of items) and `after` (the current cursor), or backwards by using `last` (numbmer of items) and `before` (the cursor).

Caliban ships with a set of abstract classes to make it easier to use Relay connections in your schema:

```scala
import caliban.relay._

// The entity you want to paginate over
case class Item(name: String)

// The specific edge type for your connection.
// The default cursor implementation is a base64 encoded index offset,
// but you can easily implement your own cursor to support e.g
// cursor based pagination from your database.
case class ItemEdge(cursor: Base64Cursor, node: Item) extends Edge[Base64Cursor, Item]

object ItemEdge {
  def apply(x: Item, i: Int): ItemEdge = ItemEdge(Base64Cursor(i), x)
}

// The top level connection itself
case class ItemConnection(
  pageInfo: PageInfo,
  edges: List[ItemEdge]
) extends Connection[ItemEdge]

object ItemConnection {
  val fromList =
    Connection.fromList(ItemConnection.apply)(ItemEdge.apply)(_, _)
}


// The arguments for your resolver.
// These are the minimal set of fields needed,
// but you can easily customize it to add e.g
// sorting or filtering.
case class Args(
  first: Option[Int],
  last: Option[Int],
  before: Option[String],
  after: Option[String]
) extends PaginationArgs[Base64Cursor]


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
```

## Cursors
It's possible to implement your own cursor type to match with the underlying data source you have. This may be a database cursor, a date offset or something else which you use to efficiently filter your result set.

Start off by implementing a case class to represent your cursor:

```scala
case class ElasticCursor(value: String)
```

To turn your case class into a usable cursor, you need to do two things: implement the `Cursor` trait and specify a schema for the case class to make sure it's always serialized as a scalar value.

First, let's implement the trait:
```scala
case class ElasticCursor(value: String)
object ElasticCursor {
  lazy val decoder = Base64.getDecoder()
  lazy val encoder = Base64.getEncoder()

  implicit val cursor: Cursor[ElasticCursor] = new Cursor[ElasticCursor] {
    type T = String
    def encode(a: ElasticCursor): String = {
      encoder.encodeToString(s"cursor:${a.value}".getBytes("UTF-8"))
    }
    def decode(s: String): Either[String, ElasticCursor] =
      Try(
        ElasticCursor(
          new String(decoder.decode(s), "UTF-8").replaceFirst("cursor:", "")
        )
      ).toEither.left.map(t => t.toString())

    def value(c: ElasticCursor): T = c.value
  }
}
```

and the schema:

```scala
  implicit val schema: Schema[Any, ElasticCursor] = Schema.scalarSchema(
    "String",
    Some("A cursor representing the current page of the pagination"),
    None,
    c => Value.StringValue(Cursor[ElasticCursor].encode(c))
  )
```