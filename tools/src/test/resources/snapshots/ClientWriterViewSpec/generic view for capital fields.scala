import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type TypeWithCapitalFields
  object TypeWithCapitalFields {

    final case class TypeWithCapitalFieldsView(Name: scala.Option[String], Value: scala.Option[String])

    type ViewSelection = SelectionBuilder[TypeWithCapitalFields, TypeWithCapitalFieldsView]

    def view: ViewSelection = (Name ~ Value).map { case (name$, value$) => TypeWithCapitalFieldsView(name$, value$) }

    def Name: SelectionBuilder[TypeWithCapitalFields, scala.Option[String]]  =
      _root_.caliban.client.SelectionBuilder.Field("Name", OptionOf(Scalar()))
    def Value: SelectionBuilder[TypeWithCapitalFields, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("Value", OptionOf(Scalar()))
  }

}
