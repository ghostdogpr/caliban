import Types._

import zio.stream.ZStream

object Types {
  final case class QueryCharactersArgs(p: Params)
  final case class SubscriptionCharactersArgs(p: Params)
  final case class Params(p: Int)

}

object Operations {

  final case class Query(
    characters: QueryCharactersArgs => zio.UIO[Int]
  )

  final case class Subscription(
    characters: SubscriptionCharactersArgs => ZStream[Any, Nothing, Int]
  )

}
