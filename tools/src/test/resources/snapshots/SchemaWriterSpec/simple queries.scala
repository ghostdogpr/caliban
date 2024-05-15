import Types._

object Types {
  final case class QueryUserArgs(id: scala.Option[Int])
  final case class User(id: scala.Option[Int], name: scala.Option[String], profilePic: scala.Option[String])

}

object Operations {

  final case class Query(
    user: QueryUserArgs => zio.UIO[scala.Option[User]],
    userList: zio.UIO[List[scala.Option[User]]]
  )

}
