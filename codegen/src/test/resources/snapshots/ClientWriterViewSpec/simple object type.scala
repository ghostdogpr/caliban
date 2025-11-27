import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view: ViewSelection = (name ~ age ~ nicknames).map { case (name, age, nicknames) =>
      CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]                = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
