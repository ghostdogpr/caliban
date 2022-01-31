package example.optimizations

import example.optimizations.CommonData._
import caliban.schema.{ GenericSchema, Schema }
import caliban.{ GraphQL, RootResolver }
import zio.Console.printLine
import zio.{ Console, ExitCode, ZIO, ZIOAppDefault }
import zio.query.DataSource.fromFunctionBatchedZIO
import zio.query.{ CompletedRequestMap, DataSource, Request, ZQuery }

/**
 * Optimized implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 8 requests.
 */
object OptimizedTest extends ZIOAppDefault with GenericSchema[Console] {

  type ConsoleQuery[A] = ZQuery[Console, Nothing, A]

  case class Queries(user: UserArgs => ConsoleQuery[User])

  case class User(
    fullName: String,
    username: String,
    picture: SizeArgs => String,
    upcomingEvents: FirstArgs => ConsoleQuery[List[Event]]
  )

  case class Event(
    id: Int,
    name: String,
    date: String,
    startTime: String,
    endTime: String,
    viewerRsvp: ConsoleQuery[ViewerMetadata],
    tags: ConsoleQuery[List[Tag]],
    venue: ConsoleQuery[Venue],
    attendingFriendsOfViewer: FirstArgs => ConsoleQuery[List[User]]
  )

  def fakeUser(id: Int) = User(
    "name",
    "name",
    args => s"picture of size ${args.size}",
    args => getUpcomingEventIdsForUser(id, args.first).flatMap(ZQuery.foreachPar(_)(getEvent))
  )

  def fakeEvent(id: Int) = Event(
    id,
    "name",
    "date",
    "start",
    "end",
    getViewerMetadataForEvent(id),
    getTags(List(1, 2, 3, 4, 5)),
    getVenue(id),
    args => getViewerFriendIdsAttendingEvent(id, args.first).flatMap(ZQuery.foreachPar(_)(getUser))
  )

  case class GetUser(id: Int) extends Request[Nothing, User]
  val UserDataSource: DataSource[Console, GetUser] = DataSource.Batched.make("UserDataSource") { requests =>
    requests.toList match {
      case head :: Nil =>
        printLine("getUser").orDie.as(CompletedRequestMap.empty.insert(head)(Right(fakeUser(head.id))))
      case list        =>
        printLine("getUsers").orDie.as(list.foldLeft(CompletedRequestMap.empty) { case (map, req) =>
          map.insert(req)(Right(fakeUser(req.id)))
        })
    }
  }

  case class GetEvent(id: Int) extends Request[Nothing, Event]
  val EventDataSource: DataSource[Console, GetEvent] =
    fromFunctionBatchedZIO("EventDataSource")(requests => printLine("getEvents").as(requests.map(r => fakeEvent(r.id))))

  case class GetViewerMetadataForEvents(id: Int) extends Request[Nothing, ViewerMetadata]
  val ViewerMetadataDataSource: DataSource[Console, GetViewerMetadataForEvents] =
    fromFunctionBatchedZIO("ViewerMetadataDataSource") { requests =>
      printLine("getViewerMetadataForEvents").as(requests.map(_ => ViewerMetadata("")))
    }

  case class GetVenue(id: Int) extends Request[Nothing, Venue]
  val VenueDataSource: DataSource[Console, GetVenue] =
    fromFunctionBatchedZIO("VenueDataSource")(requests => printLine("getVenues").as(requests.map(_ => Venue("venue"))))

  case class GetTags(ids: List[Int]) extends Request[Nothing, List[Tag]]
  val TagsDataSource: DataSource[Console, GetTags] =
    fromFunctionBatchedZIO("TagsDataSource") { requests =>
      printLine("getTags").as(requests.map(_.ids.map(id => Tag(id.toString))))
    }

  case class GetViewerFriendIdsAttendingEvent(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val ViewerFriendDataSource: DataSource[Console, GetViewerFriendIdsAttendingEvent] =
    fromFunctionBatchedZIO("ViewerFriendDataSource") { requests =>
      printLine("getViewerFriendIdsAttendingEvent").as(requests.map(r => (1 to r.first).toList))
    }

  case class GetUpcomingEventIdsForUser(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val UpcomingEventDataSource: DataSource[Console, GetUpcomingEventIdsForUser] =
    fromFunctionBatchedZIO("UpcomingEventDataSource") { requests =>
      printLine("getUpcomingEventIdsForUser").as(requests.map(r => (1 to r.first).toList))
    }

  def getUser(id: Int): ConsoleQuery[User]                                           = ZQuery.fromRequest(GetUser(id))(UserDataSource)
  def getEvent(id: Int): ConsoleQuery[Event]                                         = ZQuery.fromRequest(GetEvent(id))(EventDataSource)
  def getVenue(id: Int): ConsoleQuery[Venue]                                         = ZQuery.fromRequest(GetVenue(id))(VenueDataSource)
  def getTags(ids: List[Int]): ConsoleQuery[List[Tag]]                               = ZQuery.fromRequest(GetTags(ids))(TagsDataSource)
  def getViewerMetadataForEvent(id: Int): ConsoleQuery[ViewerMetadata]               =
    ZQuery.fromRequest(GetViewerMetadataForEvents(id))(ViewerMetadataDataSource)
  def getViewerFriendIdsAttendingEvent(id: Int, first: Int): ConsoleQuery[List[Int]] =
    ZQuery.fromRequest(GetViewerFriendIdsAttendingEvent(id, first))(ViewerFriendDataSource)
  def getUpcomingEventIdsForUser(id: Int, first: Int): ConsoleQuery[List[Int]]       =
    ZQuery.fromRequest(GetUpcomingEventIdsForUser(id, first))(UpcomingEventDataSource)

  implicit val viewerMetadataSchema: Schema[Any, ViewerMetadata] = Schema.gen
  implicit val tagSchema: Schema[Any, Tag]                       = Schema.gen
  implicit val venueSchema: Schema[Any, Venue]                   = Schema.gen
  implicit val userArgsSchema: Schema[Any, UserArgs]             = Schema.gen
  implicit val sizeArgsSchema: Schema[Any, SizeArgs]             = Schema.gen
  implicit val firstArgsSchema: Schema[Any, FirstArgs]           = Schema.gen
  implicit lazy val user: Schema[Console, User]                  = Schema.gen

  val resolver = Queries(args => getUser(args.id))
  val api      = GraphQL.graphQL(RootResolver(resolver))

  override def run: ZIO[zio.ZEnv, Nothing, ExitCode] =
    api.interpreter
      .flatMap(_.execute(query).map(res => ExitCode(res.errors.length)))
      .exitCode
}
