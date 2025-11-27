import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def _$ : SelectionBuilder[Character, scala.Option[Boolean]]      =
      _root_.caliban.client.SelectionBuilder.Field("_", OptionOf(Scalar()))
    def `_name_` : SelectionBuilder[Character, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("_name_", OptionOf(Scalar()))
    def _nickname: SelectionBuilder[Character, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("_nickname", OptionOf(Scalar()))
    def `age_` : SelectionBuilder[Character, scala.Option[Int]]      =
      _root_.caliban.client.SelectionBuilder.Field("age_", OptionOf(Scalar()))
  }

}
