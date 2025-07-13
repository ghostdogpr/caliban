import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type PriceRuleCustomerSelection
  object PriceRuleCustomerSelection {
    def customers(after: scala.Option[String] = None, savedSearchId: scala.Option[String] = None)(implicit
      encoder0: ArgEncoder[scala.Option[String]],
      encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[PriceRuleCustomerSelection, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "customers",
        OptionOf(Scalar()),
        arguments = List(Argument("after", after, "String"), Argument("savedSearchId", savedSearchId, "ID"))
      )
  }

}
