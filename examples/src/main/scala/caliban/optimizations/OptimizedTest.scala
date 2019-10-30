package caliban.optimizations

import caliban.optimizations.CommonData._
import caliban.schema.Schema
import caliban.{ GraphQL, RootResolver }
import zio.{ App, UIO, ZIO }
import zquery.DataSource.Service.fromFunctionBatchedM
import zquery.{ CompletedRequestMap, DataSource, Request, ZQuery }

/**
 * Optimized implementation of https://blog.apollographql.com/optimizing-your-graphql-request-waterfalls-7c3f3360b051
 * Will result in 8 requests.
 */
object OptimizedTest extends App {

  type Query[A] = ZQuery[Any, Nothing, A]

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

  case class GetUser(id: Int) extends Request[User]
  val UserDataSource: DataSource.Service[Any, Nothing, GetUser] = DataSource.Service("UserDataSource") { requests =>
    requests.toList match {
      case head :: Nil => UIO(println("getUser")).as(CompletedRequestMap.empty.insert(head)(fakeUser(head.id)))
      case list =>
        UIO(println("getUsers")).as(list.foldLeft(CompletedRequestMap.empty) {
          case (map, req) => map.insert(req)(fakeUser(req.id))
        })
    }
  }

  case class GetEvent(id: Int) extends Request[Event]
  val EventDataSource: DataSource.Service[Any, Nothing, GetEvent] =
    fromFunctionBatchedM("EventDataSource") { requests =>
      UIO(println("getEvents")).as(requests.map(r => fakeEvent(r.id)))
    }

  case class GetViewerMetadataForEvents(id: Int) extends Request[ViewerMetadata]
  val ViewerMetadataDataSource: DataSource.Service[Any, Nothing, GetViewerMetadataForEvents] =
    fromFunctionBatchedM("ViewerMetadataDataSource") { requests =>
      UIO(println("getViewerMetadataForEvents")).as(requests.map(_ => ViewerMetadata("")))
    }

  case class GetVenue(id: Int) extends Request[Venue]
  val VenueDataSource: DataSource.Service[Any, Nothing, GetVenue] =
    fromFunctionBatchedM("VenueDataSource") { requests =>
      UIO(println("getVenues")).as(requests.map(_ => Venue("venue")))
    }

  case class GetTags(ids: List[Int]) extends Request[List[Tag]]
  val TagsDataSource: DataSource.Service[Any, Nothing, GetTags] =
    fromFunctionBatchedM("TagsDataSource") { requests =>
      UIO(println("getTags")).as(requests.map(_.ids.map(id => Tag(id.toString))))
    }

  case class GetViewerFriendIdsAttendingEvent(id: Int, first: Int) extends Request[List[Int]]
  val ViewerFriendDataSource: DataSource.Service[Any, Nothing, GetViewerFriendIdsAttendingEvent] =
    fromFunctionBatchedM("ViewerFriendDataSource") { requests =>
      UIO(println("getViewerFriendIdsAttendingEvent")).as(requests.map(r => (1 to r.first).toList))
    }

  case class GetUpcomingEventIdsForUser(id: Int, first: Int) extends Request[List[Int]]
  val UpcomingEventDataSource: DataSource.Service[Any, Nothing, GetUpcomingEventIdsForUser] =
    fromFunctionBatchedM("UpcomingEventDataSource") { requests =>
      UIO(println("getUpcomingEventIdsForUser")).as(requests.map(r => (1 to r.first).toList))
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

  implicit lazy val user: Schema[Any, User] = Schema.gen[User]

  val resolver                                       = Queries(args => getUser(args.id))
  val interpreter: GraphQL[Any, Queries, Unit, Unit] = GraphQL.graphQL(RootResolver(resolver))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    interpreter.execute(query).catchAll(err => UIO(println(err))).as(0)
}
