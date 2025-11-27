import Types._

import zio.stream.ZStream

object Types {
  final case class SubscriptionUserWatchArgs(id: Int)

}

object Operations {

  final case class Subscription(
    UserWatch: SubscriptionUserWatchArgs => ZStream[Any, Nothing, String]
  )

}
