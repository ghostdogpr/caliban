import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def character[A](name: String)(innerSelection: SelectionBuilder[Character, A])(implicit
      encoder0: ArgEncoder[String]
    ): SelectionBuilder[Q, scala.Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("character", OptionOf(Obj(innerSelection)), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
