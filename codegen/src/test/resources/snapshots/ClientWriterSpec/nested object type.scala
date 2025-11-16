import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Q, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("characters", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
