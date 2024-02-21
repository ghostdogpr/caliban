package example.optimizations

import caliban._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import example.optimizations.CommonData._
import zio.Console.printLine
import zio.query.DataSource.fromFunctionBatchedZIO
import zio.query.{ CompletedRequestMap, DataSource, Request, ZQuery }
import zio.{ Exit, ExitCode, ZIOAppDefault }

/**
 * Optimized implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 8 requests.
 */
object OptimizedTest extends ZIOAppDefault {

  type ConsoleQuery[A] = ZQuery[Any, Nothing, A]

  case class Queries(user: UserArgs => User)

  case class User(
    fullName: ConsoleQuery[String],
    username: ConsoleQuery[String],
    picture: SizeArgs => ConsoleQuery[String],
    upcomingEvents: FirstArgs => ConsoleQuery[List[Event]]
  )

  case class Event(
    id: ConsoleQuery[Int],
    name: ConsoleQuery[String],
    date: ConsoleQuery[String],
    startTime: ConsoleQuery[String],
    endTime: ConsoleQuery[String],
    viewerRsvp: ConsoleQuery[ViewerMetadata],
    tags: ConsoleQuery[List[Tag]],
    venue: ConsoleQuery[Venue],
    attendingFriendsOfViewer: FirstArgs => ConsoleQuery[List[User]]
  )

  case class UserData(fullName: String, username: String, picture: Int => String)

  case class EventData(
    id: Int,
    name: String,
    date: String,
    startTime: String,
    endTime: String,
    venueId: Int,
    tagIds: List[Int]
  )

  def fakeUser(id: Int)  = UserData("name", "name", size => s"picture of size $size")
  def fakeEvent(id: Int) = EventData(id, "name", "date", "start", "end", 0, List(1, 2, 3, 4, 5))

  case class GetUser(id: Int) extends Request[Nothing, UserData]
  val UserDataSource: DataSource[Any, GetUser] = DataSource.Batched.make("UserDataSource") { requests =>
    requests.toList match {
      case head :: Nil =>
        printLine("getUser").orDie.as(CompletedRequestMap.empty.insert(head, Exit.succeed(fakeUser(head.id))))
      case list        =>
        printLine("getUsers").orDie.as(list.foldLeft(CompletedRequestMap.empty) { case (map, req) =>
          map.insert(req, Exit.succeed(fakeUser(req.id)))
        })
    }
  }

  case class GetEvent(id: Int) extends Request[Nothing, EventData]
  val EventDataSource: DataSource[Any, GetEvent] =
    fromFunctionBatchedZIO("EventDataSource")(requests =>
      printLine("getEvents").orDie.as(requests.map(r => fakeEvent(r.id)))
    )

  case class GetViewerMetadataForEvents(id: Int) extends Request[Nothing, ViewerMetadata]
  val ViewerMetadataDataSource: DataSource[Any, GetViewerMetadataForEvents] =
    fromFunctionBatchedZIO("ViewerMetadataDataSource") { requests =>
      printLine("getViewerMetadataForEvents").orDie.as(requests.map(_ => ViewerMetadata("")))
    }

  case class GetVenue(id: Int) extends Request[Nothing, Venue]
  val VenueDataSource: DataSource[Any, GetVenue] =
    fromFunctionBatchedZIO("VenueDataSource")(requests =>
      printLine("getVenues").orDie.as(requests.map(_ => Venue("venue")))
    )

  case class GetTags(ids: List[Int]) extends Request[Nothing, List[Tag]]
  val TagsDataSource: DataSource[Any, GetTags] =
    fromFunctionBatchedZIO("TagsDataSource") { requests =>
      printLine("getTags").orDie.as(requests.map(_.ids.map(id => Tag(id.toString))))
    }

  case class GetViewerFriendIdsAttendingEvent(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val ViewerFriendDataSource: DataSource[Any, GetViewerFriendIdsAttendingEvent] =
    fromFunctionBatchedZIO("ViewerFriendDataSource") { requests =>
      printLine("getViewerFriendIdsAttendingEvent").orDie.as(requests.map(r => (1 to r.first).toList))
    }

  case class GetUpcomingEventIdsForUser(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val UpcomingEventDataSource: DataSource[Any, GetUpcomingEventIdsForUser] =
    fromFunctionBatchedZIO("UpcomingEventDataSource") { requests =>
      printLine("getUpcomingEventIdsForUser").orDie.as(requests.map(r => (1 to r.first).toList))
    }

  def getUser(id: Int): ConsoleQuery[UserData]                                       = ZQuery.fromRequest(GetUser(id))(UserDataSource)
  def getEvent(id: Int): ConsoleQuery[EventData]                                     = ZQuery.fromRequest(GetEvent(id))(EventDataSource)
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
  implicit val eventSchema: Schema[Any, Event]                   = Schema.gen
  implicit def user: Schema[Any, User]                           = Schema.gen
  implicit val queriesSchema: Schema[Any, Queries]               = Schema.gen

  def userResolver(id: Int): User =
    User(
      getUser(id).map(_.fullName),
      getUser(id).map(_.username),
      args => getUser(id).map(_.picture(args.size)),
      args => getUpcomingEventIdsForUser(id, args.first).map(_.map(eventResolver))
    )

  def eventResolver(id: Int): Event =
    Event(
      getEvent(id).map(_.id),
      getEvent(id).map(_.name),
      getEvent(id).map(_.date),
      getEvent(id).map(_.startTime),
      getEvent(id).map(_.endTime),
      getViewerMetadataForEvent(id),
      getEvent(id).flatMap(event => getTags(event.tagIds)),
      getEvent(id).flatMap(event => getVenue(event.venueId)),
      args => getViewerFriendIdsAttendingEvent(id, args.first).map(_.map(userResolver))
    )

  val resolver = Queries(args => userResolver(args.id))
  val api      = graphQL(RootResolver(resolver))

  override def run =
    api.interpreter
      .flatMap(_.execute(query).map(res => ExitCode(res.errors.length)))
}
