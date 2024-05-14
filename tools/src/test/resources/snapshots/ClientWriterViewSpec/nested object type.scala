import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {

    final case class QView[UsersSelection](users: List[UsersSelection])

    type ViewSelection[UsersSelection] = SelectionBuilder[Q, QView[UsersSelection]]

    def view[UsersSelection](usersSelection: SelectionBuilder[User, UsersSelection]): ViewSelection[UsersSelection] =
      users(usersSelection).map(users => QView(users))

    def users[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Q, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("users", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view(nicknamesArg: scala.Option[Int] = None): ViewSelection = (name ~ age ~ nicknames(nicknamesArg)).map {
      case (name, age, nicknames) => CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def nicknames(arg: scala.Option[Int] = None)(implicit
      encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Character, List[String]] = _root_.caliban.client.SelectionBuilder
      .Field("nicknames", ListOf(Scalar()), arguments = List(Argument("arg", arg, "Int")(encoder0)))
  }

  type User
  object User {

    final case class UserView[CharactersSelection](characters: List[CharactersSelection])

    type ViewSelection[CharactersSelection] = SelectionBuilder[User, UserView[CharactersSelection]]

    def view[CharactersSelection](
      charactersName: String
    )(charactersSelection: SelectionBuilder[Character, CharactersSelection]): ViewSelection[CharactersSelection] =
      characters(charactersName)(charactersSelection).map(characters => UserView(characters))

    def characters[A](name: String)(
      innerSelection: SelectionBuilder[Character, A]
    )(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, List[A]] = _root_.caliban.client.SelectionBuilder
      .Field("characters", ListOf(Obj(innerSelection)), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

}
