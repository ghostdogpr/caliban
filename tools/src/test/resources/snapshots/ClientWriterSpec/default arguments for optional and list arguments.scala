import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def characters(first: Int, last: scala.Option[Int] = None, origins: List[scala.Option[String]] = Nil)(implicit
      encoder0: ArgEncoder[Int],
      encoder1: ArgEncoder[scala.Option[Int]],
      encoder2: ArgEncoder[List[scala.Option[String]]]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "characters",
        OptionOf(Scalar()),
        arguments = List(
          Argument("first", first, "Int!")(encoder0),
          Argument("last", last, "Int")(encoder1),
          Argument("origins", origins, "[String]!")(encoder2)
        )
      )
  }

}
