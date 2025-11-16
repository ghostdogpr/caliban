import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Time = String

  type PriceRuleCustomerSelection
  object PriceRuleCustomerSelection {
    def customers(
      after: scala.Option[String] = None,
      savedSearchId: scala.Option[String] = None,
      time: scala.Option[Time] = None,
      int: scala.Option[Int] = None,
      myInt: scala.Option[Int] = None,
      scalaInt: scala.Option[scala.Int] = None,
      predefInt: scala.Option[Predef.String] = None,
      scalaPredefString: scala.Option[scala.Predef.String] = None
    )(implicit
      encoder0: ArgEncoder[scala.Option[String]],
      encoder1: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[PriceRuleCustomerSelection, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "customers",
        OptionOf(Scalar()),
        arguments = List(
          Argument("after", after, "String"),
          Argument("savedSearchId", savedSearchId, "ID"),
          Argument("time", time, "Time"),
          Argument("int", int, "Int"),
          Argument("myInt", myInt, "MyInt"),
          Argument("scalaInt", scalaInt, "ScalaInt"),
          Argument("predefInt", predefInt, "PredefString"),
          Argument("scalaPredefString", scalaPredefString, "ScalaPredefString")
        )
      )
  }

}
