package example.optimizations

import caliban.schema.{ GenericSchema, Schema }
import caliban.schema.ArgBuilder.auto._
import caliban._
import example.optimizations.CommonData._
import zio.Console.printLine
import zio.{ ExitCode, ZIO, ZIOAppDefault }

/**
 * Naive implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 47 requests.
 */
object NaiveTest extends ZIOAppDefault with GenericSchema[Any] {

  type MyIO[A] = ZIO[Any, Nothing, A]

  case class Queries(user: UserArgs => MyIO[User])

  case class User(
    fullName: String,
    username: String,
    picture: SizeArgs => String,
    upcomingEvents: FirstArgs => MyIO[List[Event]]
  )

  case class Event(
    id: Int,
    name: String,
    date: String,
    startTime: String,
    endTime: String,
    viewerRsvp: MyIO[ViewerMetadata],
    tags: MyIO[List[Tag]],
    venue: MyIO[Venue],
    attendingFriendsOfViewer: FirstArgs => MyIO[List[User]]
  )

  def getUpcomingEventIdsForUser(id: Int, first: Int): MyIO[List[Int]] =
    printLine("getUpcomingEventIdsForUser").orDie.as((1 to first).toList)

  def getViewerMetadataForEvent(id: Int): MyIO[ViewerMetadata] =
    printLine("getViewerMetadataForEvent").orDie.as(ViewerMetadata(""))

  def getVenue(id: Int): MyIO[Venue] = printLine("getVenue").orDie.as(Venue("venue"))

  def getTags(ids: List[Int]): MyIO[List[Tag]] = printLine("getTags").orDie.as(ids.map(id => Tag(id.toString)))

  def getViewerFriendIdsAttendingEvent(id: Int, first: Int): MyIO[List[Int]] =
    printLine("getViewerFriendIdsAttendingEvent").orDie.as((1 to first).toList)

  def getEvent(id: Int): MyIO[Event] =
    printLine("getEvent").orDie.as(
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

  def getUser(id: Int): MyIO[User] =
    printLine("getUser").orDie.as(
      User(
        "name",
        "name",
        args => s"picture of size ${args.size}",
        args => getUpcomingEventIdsForUser(id, args.first).flatMap(ZIO.foreach(_)(getEvent))
      )
    )

  implicit val viewerMetadataSchema: Schema[Any, ViewerMetadata] = Schema.gen
  implicit val tagSchema: Schema[Any, Tag]                       = Schema.gen
  implicit val venueSchema: Schema[Any, Venue]                   = Schema.gen
  implicit val userArgsSchema: Schema[Any, UserArgs]             = Schema.gen
  implicit val sizeArgsSchema: Schema[Any, SizeArgs]             = Schema.gen
  implicit val firstArgsSchema: Schema[Any, FirstArgs]           = Schema.gen
  implicit val eventSchema: Schema[Any, Event]                   = Schema.gen
  implicit lazy val user: Schema[Any, User]                      = Schema.gen
  implicit val queriesSchema: Schema[Any, Queries]               = Schema.gen

  val resolver = Queries(args => getUser(args.id))
  val api      = graphQL(RootResolver(resolver))

  override def run =
    api.interpreter
      .flatMap(_.execute(query).map(res => ExitCode(res.errors.length)))
      .exitCode
}
