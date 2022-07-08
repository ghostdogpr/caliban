package poc.caliban.client.generated.potatoes

import caliban.client.FieldBuilder._
import caliban.client._

object Query {
  def byName[A](value: String)(innerSelection: SelectionBuilder[Potato, A])(
      implicit encoder0: ArgEncoder[String]
  ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[
    A
  ]] = _root_.caliban.client.SelectionBuilder.Field(
    "byName",
    OptionOf(Obj(innerSelection)),
    arguments = List(Argument("value", value, "String!")(encoder0))
  )
  def byColor[A](value: Color)(innerSelection: SelectionBuilder[Potato, A])(
      implicit encoder0: ArgEncoder[Color]
  ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[
    List[A]
  ]] = _root_.caliban.client.SelectionBuilder.Field(
    "byColor",
    OptionOf(ListOf(Obj(innerSelection))),
    arguments = List(Argument("value", value, "Color!")(encoder0))
  )
}

