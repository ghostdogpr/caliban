import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type PriceRuleCustomerSelection
  object PriceRuleCustomerSelection {
    def customers(int: scala.Option[Int] = None, myInt: scala.Option[scala.Int] = None)(implicit
      encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[PriceRuleCustomerSelection, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "customers",
        OptionOf(Scalar()),
        arguments = List(Argument("int", int, "Int"), Argument("myInt", myInt, "MyInt"))
      )
  }

}
