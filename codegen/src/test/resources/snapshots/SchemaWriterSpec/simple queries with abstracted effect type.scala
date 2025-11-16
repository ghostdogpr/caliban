import Types._

object Types {
  final case class QueryUserArgs(id: scala.Option[Int])
  final case class User(id: scala.Option[Int], name: scala.Option[String], profilePic: scala.Option[String])

}

object Operations {

  final case class Query[F[_]](
    user: QueryUserArgs => F[scala.Option[User]],
    userList: F[List[scala.Option[User]]]
  )

}
