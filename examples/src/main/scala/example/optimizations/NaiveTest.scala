package example.optimizations

import example.optimizations.CommonData._

import caliban.schema.{ GenericSchema, Schema }
import caliban.{ GraphQL, RootResolver }

import zio.console.{ putStrLn, Console }
import zio.{ App, ExitCode, ZIO }

/**
 * Naive implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 47 requests.
 */
object NaiveTest extends App with GenericSchema[Console] {

  type MyIO[A] = ZIO[Console, Nothing, A]

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
    putStrLn("getUpcomingEventIdsForUser").as((1 to first).toList)

  def getViewerMetadataForEvent(id: Int): MyIO[ViewerMetadata] =
    putStrLn("getViewerMetadataForEvent").as(ViewerMetadata(""))

  def getVenue(id: Int): MyIO[Venue] = putStrLn("getVenue").as(Venue("venue"))

  def getTags(ids: List[Int]): MyIO[List[Tag]] = putStrLn("getTags").as(ids.map(id => Tag(id.toString)))

  def getViewerFriendIdsAttendingEvent(id: Int, first: Int): MyIO[List[Int]] =
    putStrLn("getViewerFriendIdsAttendingEvent").as((1 to first).toList)

  def getEvent(id: Int): MyIO[Event] =
    putStrLn("getEvent").as(
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
    putStrLn("getUser").as(
      User(
        "name",
        "name",
        args => s"picture of size ${args.size}",
        args => getUpcomingEventIdsForUser(id, args.first).flatMap(ZIO.foreach(_)(getEvent))
      )
    )

  implicit val viewerMetadataSchema: Schema[Any, ViewerMetadata] = Schema.gen[ViewerMetadata]
  implicit val tagSchema: Schema[Any, Tag]                       = Schema.gen[Tag]
  implicit val venueSchema: Schema[Any, Venue]                   = Schema.gen[Venue]
  implicit val userArgsSchema: Schema[Any, UserArgs]             = Schema.gen[UserArgs]
  implicit val sizeArgsSchema: Schema[Any, SizeArgs]             = Schema.gen[SizeArgs]
  implicit val firstArgsSchema: Schema[Any, FirstArgs]           = Schema.gen[FirstArgs]
  implicit lazy val user: Schema[Console, User]                  = gen[User]

  val resolver = Queries(args => getUser(args.id))
  val api      = GraphQL.graphQL(RootResolver(resolver))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    api.interpreter
      .flatMap(_.execute(query).map(res => ExitCode(res.errors.length)))
      .exitCode
}
