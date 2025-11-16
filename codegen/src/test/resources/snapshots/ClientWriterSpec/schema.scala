import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

  type Q = _root_.caliban.client.Operations.RootQuery
  object Q {
    def characters[A](
      innerSelection: SelectionBuilder[Character, A]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("characters", ListOf(Obj(innerSelection)))
  }

}
