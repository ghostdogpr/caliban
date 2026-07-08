import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {

    /**
     * use bar * / instead / * here
     */
    def foo: SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("foo", OptionOf(Scalar()))
  }

}
