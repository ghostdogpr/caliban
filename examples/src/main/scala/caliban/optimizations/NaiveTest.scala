package caliban.optimizations

import caliban.optimizations.CommonData._
import caliban.schema.Schema
import caliban.{ GraphQL, RootResolver }
import zio.{ App, UIO, ZIO }

/**
 * Naive implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 47 requests.
 */
object NaiveTest extends App {

  case class Queries(user: UserArgs => UIO[User])

  case class User(
    fullName: String,
    username: String,
    picture: SizeArgs => String,
    upcomingEvents: FirstArgs => UIO[List[Event]]
  )

  case class Event(
    id: Int,
    name: String,
    date: String,
    startTime: String,
    endTime: String,
    viewerRsvp: UIO[ViewerMetadata],
    tags: UIO[List[Tag]],
    venue: UIO[Venue],
    attendingFriendsOfViewer: FirstArgs => UIO[List[User]]
  )

  def getUpcomingEventIdsForUser(id: Int, first: Int): UIO[List[Int]] =
    UIO(println("getUpcomingEventIdsForUser")).as((1 to first).toList)

  def getViewerMetadataForEvent(id: Int): UIO[ViewerMetadata] =
    UIO(println("getViewerMetadataForEvent")).as(ViewerMetadata(""))

  def getVenue(id: Int): UIO[Venue] = UIO(println("getVenue")).as(Venue("venue"))

  def getTags(ids: List[Int]): UIO[List[Tag]] = UIO(println("getTags")).as(ids.map(id => Tag(id.toString)))

  def getViewerFriendIdsAttendingEvent(id: Int, first: Int): UIO[List[Int]] =
    UIO(println("getViewerFriendIdsAttendingEvent")).as((1 to first).toList)

  def getEvent(id: Int): UIO[Event] =
    UIO(println("getEvent")).as(
      Event(
        id,
        "name",
        "date",
        "start",
        "end",
        getViewerMetadataForEvent(id),
        getTags(List(1, 2, 3, 4, 5)),
        getVenue(id),
        args => getViewerFriendIdsAttendingEvent(id, args.first).flatMap(ZIO.foreach(_)(getUser))
      )
    )

  def getUser(id: Int): UIO[User] =
    UIO(println("getUser")).as(
      User(
        "name",
        "name",
        args => s"picture of size ${args.size}",
        args => getUpcomingEventIdsForUser(id, args.first).flatMap(ZIO.foreach(_)(getEvent))
      )
    )

  implicit lazy val user: Schema[Any, User] = Schema.gen[User]

  val resolver                                       = Queries(args => getUser(args.id))
  val interpreter: GraphQL[Any, Queries, Unit, Unit] = GraphQL.graphQL(RootResolver(resolver))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    interpreter.execute(query).catchAll(err => UIO(println(err))).as(0)
}
