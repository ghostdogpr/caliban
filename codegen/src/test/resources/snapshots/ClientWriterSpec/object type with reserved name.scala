import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def `type`: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("type", Scalar())
  }

}
