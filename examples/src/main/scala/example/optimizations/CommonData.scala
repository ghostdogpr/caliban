package example.optimizations

object CommonData {
  case class UserArgs(id: Int)
  case class FirstArgs(first: Int)
  case class SizeArgs(size: Int)

  case class ViewerMetadata(name: String)
  case class Tag(name: String)
  case class Venue(name: String)

  val query: String =
    """
      |query UserEventsScreen {
      |  user(id: 1) {
      |    fullName
      |    username
      |    picture(size: 50)
      |    upcomingEvents(first: 5) {
      |      id
      |      name
      |      date
      |      startTime
      |      endTime
      |      viewerRsvp {
      |        name
      |      }
      |      tags {
      |        name
      |      }
      |      venue {
      |        name
      |      }
      |      attendingFriendsOfViewer(first: 4) {
      |        picture(size: 25)
      |      }
      |    }
      |  }
      |}""".stripMargin
}
