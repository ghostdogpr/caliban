import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def characters(
      first: Int,
      @deprecated("foo bar")
      last: scala.Option[Int] = None,
      @deprecated
      origins: scala.Option[List[scala.Option[String]]] = None
    )(implicit
      encoder0: ArgEncoder[Int],
      encoder1: ArgEncoder[scala.Option[Int]],
      encoder2: ArgEncoder[scala.Option[List[scala.Option[String]]]]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "characters",
        OptionOf(Scalar()),
        arguments = List(
          Argument("first", first, "Int!"),
          Argument("last", last, "Int"),
          Argument("origins", origins, "[String]")
        )
      )
  }

}
