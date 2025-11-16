import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Entity
  object Entity {
    def name: SelectionBuilder[Entity, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Pet
  object Pet {
    def name: SelectionBuilder[Pet, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Cat
  object Cat {
    def name: SelectionBuilder[Cat, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Dog
  object Dog {
    def name: SelectionBuilder[Dog, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

}
