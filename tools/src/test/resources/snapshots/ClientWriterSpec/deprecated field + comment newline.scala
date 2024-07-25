import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    /**
     * name
     */
    @deprecated("""foo
bar""")
    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

}
