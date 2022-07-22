package poc.caliban.client.generated.potatoes

import caliban.client.FieldBuilder._
import caliban.client._

object Potato {
  def name[A](
      innerSelection: SelectionBuilder[Name, A]
  ): SelectionBuilder[Potato, A] =
    _root_.caliban.client.SelectionBuilder.Field("name", Obj(innerSelection))
  def color: SelectionBuilder[Potato, Color] =
    _root_.caliban.client.SelectionBuilder.Field("color", Scalar())
}

