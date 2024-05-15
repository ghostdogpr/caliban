import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    /**
     * name
     */
    @deprecated("blah", "")
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    @deprecated("", "")
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
