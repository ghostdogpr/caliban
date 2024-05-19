import Types._

import zio.stream.ZStream

object Types {
  final case class MutationAddPostArgs(author: scala.Option[String], comment: scala.Option[String])
  final case class Post(author: scala.Option[String], comment: scala.Option[String])

}

object Operations {

  final case class Query(
    posts: zio.UIO[scala.Option[List[scala.Option[Post]]]]
  )

  final case class Mutation(
    addPost: MutationAddPostArgs => zio.UIO[scala.Option[Post]]
  )

  final case class Subscription(
    postAdded: ZStream[Any, Nothing, scala.Option[Post]]
  )

}
