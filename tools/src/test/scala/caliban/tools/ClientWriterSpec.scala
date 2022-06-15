package caliban.tools

import caliban.parsing.Parser
import zio.Task
import zio.test._

object ClientWriterSpec extends ZIOSpecDefault {

  def gen(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty,
    additionalImports: List[String] = List.empty,
    extensibleEnums: Boolean = false
  ): Task[String] = Parser
    .parseQuery(schema)
    .flatMap(doc =>
      Formatter.format(
        ClientWriter
          .write(
            doc,
            additionalImports = Some(additionalImports),
            extensibleEnums = extensibleEnums,
            scalarMappings = Some(scalarMappings)
          )
          .head
          ._2,
        None
      )
    )

  def genSplit(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty
  ): Task[List[(String, String)]] = Parser
    .parseQuery(schema)
    .flatMap(doc =>
      Formatter.format(
        ClientWriter.write(doc, packageName = Some("test"), splitFiles = true, scalarMappings = Some(scalarMappings)),
        None
      )
    )

  override def spec =
    suite("ClientWriterSpec")(
      test("simple object type") {
        val schema =
          """
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("object type with reserved name") {
        val schema =
          """
             type Character {
               type: String!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def `type`: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("type", Scalar())
  }

}
"""
          )
        }
      },
      test("nested object type") {
        val schema =
          """
             type Q {
               characters: [Character!]!
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Q, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("characters", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("object type with arguments") {
        val schema =
          """
             type Q {
               character(name: String!): Character
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def character[A](name: String)(
      innerSelection: SelectionBuilder[Character, A]
    )(implicit encoder0: ArgEncoder[String]): SelectionBuilder[Q, Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("character", OptionOf(Obj(innerSelection)), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("schema") {
        val schema =
          """
             schema {
               query: Q
             }

             type Q {
               characters: [Character!]!
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

  type Q = _root_.caliban.client.Operations.RootQuery
  object Q {
    def characters[A](
      innerSelection: SelectionBuilder[Character, A]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("characters", ListOf(Obj(innerSelection)))
  }

}
"""
          )
        }
      },
      test("enum") {
        val schema =
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH extends Origin { val value: String = "EARTH" }
    case object MARS  extends Origin { val value: String = "MARS"  }
    case object BELT  extends Origin { val value: String = "BELT"  }

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH => __EnumValue("EARTH")
      case Origin.MARS  => __EnumValue("MARS")
      case Origin.BELT  => __EnumValue("BELT")
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS, BELT)
  }

}
"""
          )
        }
      },
      test("scalar mapped enum") {
        val schema =
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }

             enum Destination {
               EARTH
               MARS
               BELT
             }

             input Routes {
               origin: Origin!
               destinations: [Destination!]!
             }
            """.stripMargin

        gen(schema, scalarMappings = Map("Destination" -> "com.example.Destination")).map { str =>
          assertTrue(
            str ==
              """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH extends Origin { val value: String = "EARTH" }
    case object MARS  extends Origin { val value: String = "MARS"  }
    case object BELT  extends Origin { val value: String = "BELT"  }

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH => __EnumValue("EARTH")
      case Origin.MARS  => __EnumValue("MARS")
      case Origin.BELT  => __EnumValue("BELT")
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS, BELT)
  }

  final case class Routes(origin: Origin, destinations: List[com.example.Destination] = Nil)
  object Routes {
    implicit val encoder: ArgEncoder[Routes] = new ArgEncoder[Routes] {
      override def encode(value: Routes): __Value =
        __ObjectValue(
          List(
            "origin"       -> implicitly[ArgEncoder[Origin]].encode(value.origin),
            "destinations" -> __ListValue(
              value.destinations.map(value => implicitly[ArgEncoder[com.example.Destination]].encode(value))
            )
          )
        )
    }
  }

}
"""
          )
        }
      },
      test("extensible enum") {
        val schema =
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """.stripMargin

        gen(schema, extensibleEnums = true).map { str =>
          assertTrue(
            str ==
              """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH                         extends Origin { val value: String = "EARTH" }
    case object MARS                          extends Origin { val value: String = "MARS"  }
    case object BELT                          extends Origin { val value: String = "BELT"  }
    final case class __Unknown(value: String) extends Origin

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case __StringValue(other)   => Right(Origin.__Unknown(other))
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH            => __EnumValue("EARTH")
      case Origin.MARS             => __EnumValue("MARS")
      case Origin.BELT             => __EnumValue("BELT")
      case Origin.__Unknown(value) => __EnumValue(value)
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS, BELT)
  }

}
"""
          )
        }
      },
      test("input object") {
        val schema =
          """
             input CharacterInput {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client._
import caliban.client.__Value._

object Client {

  final case class CharacterInput(name: String, nicknames: List[String] = Nil)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(
          List(
            "name"      -> implicitly[ArgEncoder[String]].encode(value.name),
            "nicknames" -> __ListValue(value.nicknames.map(value => implicitly[ArgEncoder[String]].encode(value)))
          )
        )
    }
  }

}
"""
          )
        }
      },
      test("input object with reserved name") {
        val schema =
          """
             input CharacterInput {
               wait: String!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client._
import caliban.client.__Value._

object Client {

  final case class CharacterInput(wait$ : String)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(List("wait" -> implicitly[ArgEncoder[String]].encode(value.wait$)))
    }
  }

}
"""
          )
        }
      },
      test("union") {
        val schema =
          """
             union Role = Captain | Pilot

             type Captain {
               shipName: String!
             }

             type Pilot {
               shipName: String!
             }

             type Character {
               role: Role
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Captain
  object Captain {
    def shipName: SelectionBuilder[Captain, String] = _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

  type Pilot
  object Pilot {
    def shipName: SelectionBuilder[Pilot, String] = _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

  type Character
  object Character {
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("role", OptionOf(ChoiceOf(Map("Captain" -> Obj(onCaptain), "Pilot" -> Obj(onPilot)))))
    def roleOption[A](
      onCaptain: Option[SelectionBuilder[Captain, A]] = None,
      onPilot: Option[SelectionBuilder[Pilot, A]] = None
    ): SelectionBuilder[Character, Option[Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
      "role",
      OptionOf(
        ChoiceOf(
          Map(
            "Captain" -> onCaptain.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a))),
            "Pilot"   -> onPilot.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a)))
          )
        )
      )
    )
  }

}
"""
          )
        }
      },
      test("deprecated field + comment") {
        val schema =
          """
             type Character {
               "name"
               name: String! @deprecated(reason: "blah")
               nicknames: [String!]! @deprecated
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    /**
     * name
     */
    @deprecated("blah", "")
    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    @deprecated("", "")
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("deprecated field + comment newline") {
        val tripleQuotes = "\"\"\""
        val schema       =
          """
             type Character {
               "name"
               name: String! @deprecated(reason: "foo\nbar")
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              s"""import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    /**
     * name
     */
    @deprecated(
      ${tripleQuotes}foo
bar$tripleQuotes,
      ""
    )
    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

}
"""
          )
        }
      },
      test("default arguments for optional and list arguments") {
        val schema =
          """
              type Query {
                characters(
                  first: Int!
                  last: Int
                  origins: [String]!
                ): String
              }""".stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def characters(first: Int, last: Option[Int] = None, origins: List[Option[String]] = Nil)(implicit
      encoder0: ArgEncoder[Int],
      encoder1: ArgEncoder[Option[Int]],
      encoder2: ArgEncoder[List[Option[String]]]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, Option[String]] =
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
"""
          )
        }
      },
      test("support for Json scalar") {
        val schema =
          """
              scalar Json

              type Query {
                test: Json!
              }""".stripMargin

        gen(schema, Map("Json" -> "io.circe.Json")).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def test: SelectionBuilder[_root_.caliban.client.Operations.RootQuery, io.circe.Json] =
      _root_.caliban.client.SelectionBuilder.Field("test", Scalar())
  }

}
"""
          )
        }
      },
      test("case-sensitive name uniqueness in enum's values") {
        val schema =
          """
              enum Episode {
                NEWHOPE
                EMPIRE
                JEDI
                jedi
              }
            """.stripMargin
        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Episode extends scala.Product with scala.Serializable { def value: String }
  object Episode {
    case object NEWHOPE extends Episode { val value: String = "NEWHOPE" }
    case object EMPIRE  extends Episode { val value: String = "EMPIRE"  }
    case object JEDI    extends Episode { val value: String = "JEDI"    }
    case object jedi_1  extends Episode { val value: String = "jedi_1"  }

    implicit val decoder: ScalarDecoder[Episode] = {
      case __StringValue("NEWHOPE") => Right(Episode.NEWHOPE)
      case __StringValue("EMPIRE")  => Right(Episode.EMPIRE)
      case __StringValue("JEDI")    => Right(Episode.JEDI)
      case __StringValue("jedi")    => Right(Episode.jedi_1)
      case other                    => Left(DecodingError(s"Can't build Episode from input $other"))
    }
    implicit val encoder: ArgEncoder[Episode]    = {
      case Episode.NEWHOPE => __EnumValue("NEWHOPE")
      case Episode.EMPIRE  => __EnumValue("EMPIRE")
      case Episode.JEDI    => __EnumValue("JEDI")
      case Episode.jedi_1  => __EnumValue("jedi")
    }

    val values: scala.collection.immutable.Vector[Episode] =
      scala.collection.immutable.Vector(NEWHOPE, EMPIRE, JEDI, jedi_1)
  }

}
"""
          )
        }
      },
      test("case-insensitive name uniqueness in 2 basic objects") {
        val schema =
          """
             type Character {
               name: String!
               nicknames: [String!]!
             }

             type character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

  type character_1
  object character_1 {
    def name: SelectionBuilder[character_1, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[character_1, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("safe names with leading and tailing _") {
        val schema =
          """
             type Character {
               _name_: String
               _nickname: String
             }
            """.stripMargin

        gen(schema).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def `_name_` : SelectionBuilder[Character, Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("_name_", OptionOf(Scalar()))
    def _nickname: SelectionBuilder[Character, Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("_nickname", OptionOf(Scalar()))
  }

}
"""
          )
        }
      },
      test("add scalar mappings and additional imports") {
        val schema =
          """
             scalar OffsetDateTime

             type Order {
               date: OffsetDateTime!
             }
            """.stripMargin

        gen(schema, Map("OffsetDateTime" -> "java.time.OffsetDateTime"), List("java.util.UUID", "a.b._")).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
import caliban.client._

import java.util.UUID
import a.b._

object Client {

  type Order
  object Order {
    def date: SelectionBuilder[Order, java.time.OffsetDateTime] =
      _root_.caliban.client.SelectionBuilder.Field("date", Scalar())
  }

}
"""
          )
        }
      },
      test("schema with splitFiles") {
        val schema =
          """
             schema {
               query: Q
             }

             type Q {
               characters: [Character!]!
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        genSplit(schema).map { ls =>
          assertTrue(
            ls ==
              List(
                "package"   -> """package object test {
                               |  type Character
                               |  type Q = _root_.caliban.client.Operations.RootQuery
                               |}
                               |""".stripMargin,
                "Character" -> """package test
                                 |
                                 |import caliban.client.FieldBuilder._
                                 |import caliban.client._
                                 |
                                 |object Character {
                                 |  def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
                                 |  def nicknames: SelectionBuilder[Character, List[String]] =
                                 |    _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
                                 |}
                                 |""".stripMargin,
                "Q"         -> """package test
                         |
                         |import caliban.client.FieldBuilder._
                         |import caliban.client._
                         |
                         |object Q {
                         |  def characters[A](
                         |    innerSelection: SelectionBuilder[Character, A]
                         |  ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, List[A]] =
                         |    _root_.caliban.client.SelectionBuilder.Field("characters", ListOf(Obj(innerSelection)))
                         |}
                         |""".stripMargin
              )
          )
        }
      },
      test("interface") {
        val schema =
          """
             interface Order {
               name: String!
             }
             type Ascending implements Order {
               name: String!
             }
             type Sort {
               object: Order
             }
            """.stripMargin

        gen(schema, Map.empty, List.empty).map { str =>
          assertTrue(
            str ==
              """import caliban.client.FieldBuilder._
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
    def `object`[A](onAscending: SelectionBuilder[Ascending, A]): SelectionBuilder[Sort, Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("object", OptionOf(ChoiceOf(Map("Ascending" -> Obj(onAscending)))))
    def objectOption[A](
      onAscending: Option[SelectionBuilder[Ascending, A]] = None
    ): SelectionBuilder[Sort, Option[Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
      "object",
      OptionOf(
        ChoiceOf(Map("Ascending" -> onAscending.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a)))))
      )
    )
    def objectInterface[A](`object`: SelectionBuilder[Order, A]): SelectionBuilder[Sort, Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("object", OptionOf(Obj(`object`)))
  }

}
"""
          )
        }
      },
      test("interface with list") {
        val schema =
          """
             interface Order {
               name: String!
             }
             type Ascending implements Order {
               name: String!
             }
             type Descending implements Order {
               name: String!
             }
             type Sort {
               orders: [Order]
             }
            """.stripMargin

        for {
          code <- gen(schema, Map.empty, List.empty)
        } yield assertTrue(
          code == """import caliban.client.FieldBuilder._
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
    ): SelectionBuilder[Sort, Option[List[Option[A]]]] = _root_.caliban.client.SelectionBuilder.Field(
      "orders",
      OptionOf(ListOf(OptionOf(ChoiceOf(Map("Ascending" -> Obj(onAscending), "Descending" -> Obj(onDescending))))))
    )
    def ordersOption[A](
      onAscending: Option[SelectionBuilder[Ascending, A]] = None,
      onDescending: Option[SelectionBuilder[Descending, A]] = None
    ): SelectionBuilder[Sort, Option[List[Option[Option[A]]]]] = _root_.caliban.client.SelectionBuilder.Field(
      "orders",
      OptionOf(
        ListOf(
          OptionOf(
            ChoiceOf(
              Map(
                "Ascending"  -> onAscending.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a))),
                "Descending" -> onDescending.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a)))
              )
            )
          )
        )
      )
    )
    def ordersInterface[A](orders: SelectionBuilder[Order, A]): SelectionBuilder[Sort, Option[List[Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("orders", OptionOf(ListOf(OptionOf(Obj(orders)))))
  }

}
"""
        )
      }
    ) @@ TestAspect.sequential
}
