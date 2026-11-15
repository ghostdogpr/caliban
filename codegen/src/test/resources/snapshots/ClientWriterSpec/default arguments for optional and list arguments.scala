import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client._
import caliban.client.__Value._

object Client {

  type Date = String

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin       {
    case object EARTH extends Origin { val value: String = "EARTH" }
    case object MARS  extends Origin { val value: String = "MARS"  }

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH => __EnumValue("EARTH")
      case Origin.MARS  => __EnumValue("MARS")
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS)
  }

  final case class Filter(
    userId: String,
    isExternal: Boolean = false,
    limit: scala.Option[Int] = Some(10),
    origins: List[Origin] = List(Origin.MARS),
    json: scala.Option[io.circe.Json] = None
  )
  object Filter       {
    implicit val encoder: ArgEncoder[Filter] = new ArgEncoder[Filter] {
      override def encode(value: Filter): __Value =
        __ObjectValue(
          List(
            "userId"     -> implicitly[ArgEncoder[String]].encode(value.userId),
            "isExternal" -> implicitly[ArgEncoder[Boolean]].encode(value.isExternal),
            "limit"      -> value.limit.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[Int]].encode(value)),
            "origins"    -> __ListValue(value.origins.map(value => implicitly[ArgEncoder[Origin]].encode(value))),
            "json"       -> value.json.fold(__NullValue: __Value)(value =>
              implicitly[ArgEncoder[io.circe.Json]].encode(value)
            )
          )
        )
    }
  }
  sealed trait Search {
    protected def encode: __Value
  }
  object Search       {
    final case class Name(name: String)         extends Search {
      protected def encode: __Value = __ObjectValue(List("name" -> implicitly[ArgEncoder[String]].encode(name)))
    }
    final case class ByFilter(byFilter: Filter) extends Search {
      protected def encode: __Value = __ObjectValue(List("byFilter" -> implicitly[ArgEncoder[Filter]].encode(byFilter)))
    }

    implicit val encoder: ArgEncoder[Search] = new ArgEncoder[Search] {
      override def encode(value: Search): __Value = value.encode
    }
  }
  type Query = _root_.caliban.client.Operations.RootQuery
  object Query        {
    def characters(
      first: Int,
      last: scala.Option[Int] = None,
      origins: List[scala.Option[String]] = Nil,
      count: Long = 5L,
      ratio: Double = 1.0,
      weight: scala.Option[BigDecimal] = Some(BigDecimal("0.5")),
      name: String = "a \"b\"\n",
      nickname: scala.Option[String] = None,
      origin: Origin = Origin.EARTH,
      date: scala.Option[Date] = Some("2020-01-01"),
      filter: Filter = Filter(
        userId = "u",
        isExternal = false,
        limit = Some(10),
        origins = List(Origin.EARTH, Origin.MARS),
        json = None
      ),
      filters: scala.Option[List[Filter]] = Some(
        List(
          Filter(userId = "u", isExternal = false, limit = None, origins = List(Origin.MARS), json = None),
          Filter(userId = "v", isExternal = true, limit = Some(10), origins = List(Origin.MARS), json = None)
        )
      ),
      search: scala.Option[Search] = Some(Search.Name("n")),
      json: io.circe.Json
    )(implicit
      encoder0: ArgEncoder[Int],
      encoder1: ArgEncoder[scala.Option[Int]],
      encoder2: ArgEncoder[List[scala.Option[String]]],
      encoder3: ArgEncoder[Long],
      encoder4: ArgEncoder[Double],
      encoder5: ArgEncoder[scala.Option[BigDecimal]],
      encoder6: ArgEncoder[String],
      encoder7: ArgEncoder[scala.Option[String]],
      encoder8: ArgEncoder[Filter],
      encoder9: ArgEncoder[scala.Option[List[Filter]]],
      encoder10: ArgEncoder[scala.Option[Search]],
      encoder11: ArgEncoder[io.circe.Json]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "characters",
        OptionOf(Scalar()),
        arguments = List(
          Argument("first", first, "Int!"),
          Argument("last", last, "Int"),
          Argument("origins", origins, "[String]!"),
          Argument("count", count, "Long!"),
          Argument("ratio", ratio, "Float!"),
          Argument("weight", weight, "BigDecimal"),
          Argument("name", name, "String!"),
          Argument("nickname", nickname, "String"),
          Argument("origin", origin, "Origin!"),
          Argument("date", date, "Date"),
          Argument("filter", filter, "Filter!"),
          Argument("filters", filters, "[Filter!]"),
          Argument("search", search, "Search"),
          Argument("json", json, "Json!")
        )
      )
  }

}
