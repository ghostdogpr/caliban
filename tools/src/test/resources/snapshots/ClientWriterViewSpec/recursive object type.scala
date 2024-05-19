import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView[FriendsSelection](name: String, age: Int, friends: List[FriendsSelection])

    type ViewSelection[FriendsSelection] = SelectionBuilder[Character, CharacterView[FriendsSelection]]

    def view[FriendsSelection](friendsFilter: scala.Option[String] = None)(
      friendsSelection: SelectionBuilder[Character, FriendsSelection]
    ): ViewSelection[FriendsSelection] = (name ~ age ~ friends(friendsFilter)(friendsSelection)).map {
      case (name, age, friends) => CharacterView(name, age, friends)
    }

    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def friends[A](filter: scala.Option[String] = None)(innerSelection: SelectionBuilder[Character, A])(implicit
      encoder0: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[Character, List[A]] = _root_.caliban.client.SelectionBuilder
      .Field("friends", ListOf(Obj(innerSelection)), arguments = List(Argument("filter", filter, "String")(encoder0)))
  }

}
