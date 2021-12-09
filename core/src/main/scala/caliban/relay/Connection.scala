package caliban.relay

case class PageInfo(
  hasNextPage: Boolean,
  hasPreviousPage: Boolean,
  startCursor: Option[String],
  endCursor: Option[String]
)

abstract class Edge[+C: Cursor, +T] {
  def cursor: C
  def node: T

  def encodeCursor = Cursor[C].encode(cursor)
}

abstract class Connection[T <: Edge[_, _]] {
  val pageInfo: PageInfo
  val edges: List[T]
}

object Connection {
  def fromList[A, E <: Edge[Base64Cursor, _], R <: Connection[E]](
    f: (PageInfo, List[E]) => R
  )(g: (A, Int) => E)(
    items: List[A],
    args: Pagination[Base64Cursor]
  ): R = {
    val itemsWithIndex = items.zipWithIndex

    val sliced = (args.cursor match {
      case PaginationCursor.NoCursor       => itemsWithIndex
      case PaginationCursor.After(cursor)  => itemsWithIndex.drop(cursor.value)
      case PaginationCursor.Before(cursor) => itemsWithIndex.dropRight(cursor.value)
    })

    val dropped = args.count match {
      case PaginationCount.First(count) => sliced.take(count)
      case PaginationCount.Last(count)  => sliced.takeRight(count)
    }

    val edges = dropped.map(g.tupled)

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
    f(pageInfo, edges)
  }
}
