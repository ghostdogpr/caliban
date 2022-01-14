package caliban.relay

/**
 * The Relay PageInfo type which models pagination info
 * for a connection.
 */
case class PageInfo(
  hasNextPage: Boolean,
  hasPreviousPage: Boolean,
  startCursor: Option[String],
  endCursor: Option[String]
)

/**
 * An abstract class representing a Relay connection edge
 * for some type `T`.
 */
abstract class Edge[+C: Cursor, +T] {
  def cursor: C
  def node: T

  def encodeCursor = Cursor[C].encode(cursor)
}

/**
 * An abstract class representing a Relay connection for
 * some edge `T`.
 */
abstract class Connection[T <: Edge[_, _]] {
  val pageInfo: PageInfo
  val edges: List[T]
}

object Connection {

  /**
   * A function that returns a sliced result set based
   * on some mapping functions, a full input list of entities
   * and pagination information.
   *
   * @param makeConnection translates a [[caliban.relay.PageInfo]] object and a list of edges to a `Connection`
   * @param makeEdge translates an entity and an offset to an `Edge`
   * @param items the list of items to paginate
   * @param args a set of [[caliban.relay.Pagination]] arguments
   * @return a paginated connection
   */
  def fromList[A, E <: Edge[Base64Cursor, _], C <: Connection[E]](
    makeConnection: (PageInfo, List[E]) => C
  )(makeEdge: (A, Int) => E)(
    items: List[A],
    args: Pagination[Base64Cursor]
  ): C = {
    val itemsWithIndex = items.zipWithIndex

    val sliced = (args.cursor match {
      case PaginationCursor.NoCursor       => itemsWithIndex
      case PaginationCursor.After(cursor)  => itemsWithIndex.drop(cursor.value + 1)
      case PaginationCursor.Before(cursor) => itemsWithIndex.dropRight(cursor.value + 1)
    })

    val dropped = args.count match {
      case PaginationCount.First(count) => sliced.take(count)
      case PaginationCount.Last(count)  => sliced.takeRight(count)
    }

    val edges = dropped.map(makeEdge.tupled)

    val pageInfo = PageInfo(
      hasNextPage = edges.headOption
        .map(e => e.cursor.value + args.count.count < items.size)
        .getOrElse(false),
      hasPreviousPage = edges.lastOption
        .map(e => e.cursor.value > args.count.count)
        .getOrElse(false),
      startCursor = edges.headOption.map(start => start.encodeCursor),
      endCursor = edges.lastOption.map(end => end.encodeCursor)
    )
    makeConnection(pageInfo, edges)
  }
}
