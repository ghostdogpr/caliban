import caliban.schema.Annotations._

object Types {
  final case class CharacterFriendsConnectionArgs(first: scala.Option[Int], after: scala.Option[ID])
      derives caliban.schema.Schema.SemiAuto,
        caliban.schema.ArgBuilder
  final case class Human(friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection) extends Character
      derives caliban.schema.Schema.SemiAuto
  final case class Droid(friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection) extends Character
      derives caliban.schema.Schema.SemiAuto

  @GQLInterface
  sealed trait Character extends scala.Product with scala.Serializable derives caliban.schema.Schema.SemiAuto {
    def friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection
  }

}
