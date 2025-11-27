import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def test: SelectionBuilder[_root_.caliban.client.Operations.RootQuery, io.circe.Json] =
      _root_.caliban.client.SelectionBuilder.Field("test", Scalar())
  }

}
