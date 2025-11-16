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

  type Descending
  object Descending {
    def name: SelectionBuilder[Descending, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type Sort
  object Sort {
    def orders[A](
      onAscending: SelectionBuilder[Ascending, A],
      onDescending: SelectionBuilder[Descending, A]
    ): SelectionBuilder[Sort, scala.Option[List[scala.Option[A]]]] = _root_.caliban.client.SelectionBuilder.Field(
      "orders",
      OptionOf(ListOf(OptionOf(ChoiceOf(Map("Ascending" -> Obj(onAscending), "Descending" -> Obj(onDescending))))))
    )
    def ordersOption[A](
      onAscending: scala.Option[SelectionBuilder[Ascending, A]] = None,
      onDescending: scala.Option[SelectionBuilder[Descending, A]] = None
    ): SelectionBuilder[Sort, scala.Option[List[scala.Option[scala.Option[A]]]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "orders",
        OptionOf(
          ListOf(
            OptionOf(
              ChoiceOf(
                Map(
                  "Ascending"  -> onAscending.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
                  "Descending" -> onDescending.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
                )
              )
            )
          )
        )
      )
    def ordersInterface[A](
      orders: SelectionBuilder[Order, A]
    ): SelectionBuilder[Sort, scala.Option[List[scala.Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("orders", OptionOf(ListOf(OptionOf(Obj(orders)))))
  }

}
