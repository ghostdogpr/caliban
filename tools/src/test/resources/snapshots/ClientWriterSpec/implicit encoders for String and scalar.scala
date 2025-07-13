import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Time = String

  type PriceRuleCustomerSelection
  object PriceRuleCustomerSelection {
    def customers(
      after: scala.Option[String] = None,
      savedSearchId: scala.Option[String] = None,
      time: scala.Option[Time] = None
    )(implicit
      encoder0: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[PriceRuleCustomerSelection, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "customers",
        OptionOf(Scalar()),
        arguments = List(
          Argument("after", after, "String"),
          Argument("savedSearchId", savedSearchId, "ID"),
          Argument("time", time, "Time")
        )
      )
  }

}
