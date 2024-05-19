import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView(name: String)

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view: ViewSelection = name.map(name => CharacterView(name))

    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

}
