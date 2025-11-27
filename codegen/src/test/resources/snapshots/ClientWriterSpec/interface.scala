import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Order
  object Order {
    def name: SelectionBuilder[Order, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Ascending
  object Ascending {
    def name: SelectionBuilder[Ascending, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Sort
  object Sort {
    def `object`[A](onAscending: SelectionBuilder[Ascending, A]): SelectionBuilder[Sort, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("object", OptionOf(ChoiceOf(Map("Ascending" -> Obj(onAscending)))))
    def objectOption[A](
      onAscending: scala.Option[SelectionBuilder[Ascending, A]] = None
    ): SelectionBuilder[Sort, scala.Option[scala.Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
      "object",
      OptionOf(
        ChoiceOf(Map("Ascending" -> onAscending.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))))
      )
    )
    def objectInterface[A](`object`: SelectionBuilder[Order, A]): SelectionBuilder[Sort, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("object", OptionOf(Obj(`object`)))
  }

}
