import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type PriceRuleCustomerSelection
  object PriceRuleCustomerSelection {
    def customers(str: scala.Option[String] = None, myInt: scala.Option[scala.Int] = None)(implicit
      encoder0: ArgEncoder[scala.Option[String]],
      encoder1: ArgEncoder[scala.Option[scala.Int]]
    ): SelectionBuilder[PriceRuleCustomerSelection, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "customers",
        OptionOf(Scalar()),
        arguments = List(Argument("str", str, "String"), Argument("myInt", myInt, "MyInt"))
      )
  }

}
