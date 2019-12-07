package caliban.optimizations

import caliban.optimizations.CommonData._
import caliban.schema.{ GenericSchema, Schema }
import caliban.{ GraphQL, RootResolver }
import zio.console.{ putStrLn, Console }
import zio.{ App, ZIO }
import zquery.DataSource.Service.fromFunctionBatchedM
import zquery.{ CompletedRequestMap, DataSource, Request, ZQuery }

/**
 * Optimized implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 8 requests.
 */
object OptimizedTest extends App with GenericSchema[Console] {

  type Query[A] = ZQuery[Console, Nothing, A]

  case class Queries(user: UserArgs => Query[User])

  case class User(
    fullName: String,
    username: String,
    picture: SizeArgs => String,
    upcomingEvents: FirstArgs => Query[List[Event]]
  )

  case class Event(
    id: Int,
    name: String,
    date: String,
    startTime: String,
    endTime: String,
    viewerRsvp: Query[ViewerMetadata],
    tags: Query[List[Tag]],
    venue: Query[Venue],
    attendingFriendsOfViewer: FirstArgs => Query[List[User]]
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
  val UserDataSource: DataSource.Service[Console, GetUser] = DataSource.Service("UserDataSource") { requests =>
    requests.toList match {
      case head :: Nil => putStrLn("getUser").as(CompletedRequestMap.empty.insert(head)(Right(fakeUser(head.id))))
      case list =>
        putStrLn("getUsers").as(list.foldLeft(CompletedRequestMap.empty) {
          case (map, req) => map.insert(req)(Right(fakeUser(req.id)))
        })
    }
  }

  case class GetEvent(id: Int) extends Request[Nothing, Event]
  val EventDataSource: DataSource.Service[Console, GetEvent] =
    fromFunctionBatchedM("EventDataSource") { requests =>
      putStrLn("getEvents").as(requests.map(r => fakeEvent(r.id)))
    }

  case class GetViewerMetadataForEvents(id: Int) extends Request[Nothing, ViewerMetadata]
  val ViewerMetadataDataSource: DataSource.Service[Console, GetViewerMetadataForEvents] =
    fromFunctionBatchedM("ViewerMetadataDataSource") { requests =>
      putStrLn("getViewerMetadataForEvents").as(requests.map(_ => ViewerMetadata("")))
    }

  case class GetVenue(id: Int) extends Request[Nothing, Venue]
  val VenueDataSource: DataSource.Service[Console, GetVenue] =
    fromFunctionBatchedM("VenueDataSource") { requests =>
      putStrLn("getVenues").as(requests.map(_ => Venue("venue")))
    }

  case class GetTags(ids: List[Int]) extends Request[Nothing, List[Tag]]
  val TagsDataSource: DataSource.Service[Console, GetTags] =
    fromFunctionBatchedM("TagsDataSource") { requests =>
      putStrLn("getTags").as(requests.map(_.ids.map(id => Tag(id.toString))))
    }

  case class GetViewerFriendIdsAttendingEvent(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val ViewerFriendDataSource: DataSource.Service[Console, GetViewerFriendIdsAttendingEvent] =
    fromFunctionBatchedM("ViewerFriendDataSource") { requests =>
      putStrLn("getViewerFriendIdsAttendingEvent").as(requests.map(r => (1 to r.first).toList))
    }

  case class GetUpcomingEventIdsForUser(id: Int, first: Int) extends Request[Nothing, List[Int]]
  val UpcomingEventDataSource: DataSource.Service[Console, GetUpcomingEventIdsForUser] =
    fromFunctionBatchedM("UpcomingEventDataSource") { requests =>
      putStrLn("getUpcomingEventIdsForUser").as(requests.map(r => (1 to r.first).toList))
    }

  def getUser(id: Int): Query[User]             = ZQuery.fromRequestWith(GetUser(id))(UserDataSource)
  def getEvent(id: Int): Query[Event]           = ZQuery.fromRequestWith(GetEvent(id))(EventDataSource)
  def getVenue(id: Int): Query[Venue]           = ZQuery.fromRequestWith(GetVenue(id))(VenueDataSource)
  def getTags(ids: List[Int]): Query[List[Tag]] = ZQuery.fromRequestWith(GetTags(ids))(TagsDataSource)
  def getViewerMetadataForEvent(id: Int): Query[ViewerMetadata] =
    ZQuery.fromRequestWith(GetViewerMetadataForEvents(id))(ViewerMetadataDataSource)
  def getViewerFriendIdsAttendingEvent(id: Int, first: Int): Query[List[Int]] =
    ZQuery.fromRequestWith(GetViewerFriendIdsAttendingEvent(id, first))(ViewerFriendDataSource)
  def getUpcomingEventIdsForUser(id: Int, first: Int): Query[List[Int]] =
    ZQuery.fromRequestWith(GetUpcomingEventIdsForUser(id, first))(UpcomingEventDataSource)

  implicit val viewerMetadataSchema: Schema[Any, ViewerMetadata] = Schema.gen[ViewerMetadata]
  implicit val tagSchema: Schema[Any, Tag]                       = Schema.gen[Tag]
  implicit val venueSchema: Schema[Any, Venue]                   = Schema.gen[Venue]
  implicit val userArgsSchema: Schema[Any, UserArgs]             = Schema.gen[UserArgs]
  implicit val sizeArgsSchema: Schema[Any, SizeArgs]             = Schema.gen[SizeArgs]
  implicit val firstArgsSchema: Schema[Any, FirstArgs]           = Schema.gen[FirstArgs]
  implicit lazy val user: Schema[Console, User]                  = gen[User]

  val resolver    = Queries(args => getUser(args.id))
  val interpreter = GraphQL.graphQL(RootResolver(resolver))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    interpreter.execute(query).map(_.errors.length)
}
