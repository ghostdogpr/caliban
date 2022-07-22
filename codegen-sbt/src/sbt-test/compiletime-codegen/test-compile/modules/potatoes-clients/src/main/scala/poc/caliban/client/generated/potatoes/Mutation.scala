package poc.caliban.client.generated.potatoes

import caliban.client.FieldBuilder._
import caliban.client._

object Mutation {
  def makeNewSpecies[A](name: NameInput, color: Color)(
      innerSelection: SelectionBuilder[Potato, A]
  )(implicit
      encoder0: ArgEncoder[NameInput],
      encoder1: ArgEncoder[Color]
  ): SelectionBuilder[
    _root_.caliban.client.Operations.RootMutation,
    scala.Option[A]
  ] = _root_.caliban.client.SelectionBuilder.Field(
    "makeNewSpecies",
    OptionOf(Obj(innerSelection)),
    arguments = List(
      Argument("name", name, "NameInput!")(encoder0),
      Argument("color", color, "Color!")(encoder1)
    )
  )
  def eradicate(
      value: String
  )(implicit encoder0: ArgEncoder[String]): SelectionBuilder[
    _root_.caliban.client.Operations.RootMutation,
    scala.Option[Unit]
  ] = _root_.caliban.client.SelectionBuilder.Field(
    "eradicate",
    OptionOf(Scalar()),
    arguments = List(Argument("value", value, "String!")(encoder0))
  )
}

