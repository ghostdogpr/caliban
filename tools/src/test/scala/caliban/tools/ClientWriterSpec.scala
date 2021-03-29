package caliban.tools

import caliban.parsing.Parser
import caliban.tools.implicits.ScalarMappings
import zio.Task
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ClientWriterSpec extends DefaultRunnableSpec {

  def gen(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty,
    additionalImports: List[String] = List.empty
  ): Task[String] = Parser
    .parseQuery(schema)
    .flatMap(doc =>
      Formatter.format(
        ClientWriter.write(doc, additionalImports = Some(additionalImports))(
          ScalarMappings(Some(scalarMappings))
        ),
        None
      )
    )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ClientWriterSpec")(
      testM("simple object type") {
        val schema =
          """
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("object type with reserved name") {
        val schema =
          """
             type Character {
               type: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def `type`: SelectionBuilder[Character, String] = Field("type", Scalar())
  }

}
"""
          )
        )
      },
      testM("nested object type") {
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Q, List[A]] =
      Field("characters", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("object type with arguments") {
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Q
  object Q {
    def character[A](name: String)(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Q, Option[A]] =
      Field("character", OptionOf(Obj(innerSelection)), arguments = List(Argument("name", name)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("schema") {
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

  type Q = RootQuery
  object Q {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[RootQuery, List[A]] =
      Field("characters", ListOf(Obj(innerSelection)))
  }

}
"""
          )
        )
      },
      testM("enum") {
        val schema =
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable
  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = new ArgEncoder[Origin] {
      override def encode(value: Origin): __Value = value match {
        case Origin.EARTH => __EnumValue("EARTH")
        case Origin.MARS  => __EnumValue("MARS")
        case Origin.BELT  => __EnumValue("BELT")
      }
      override def typeName: String               = "Origin"
    }
  }

}
"""
          )
        )
      },
      testM("input object") {
        val schema =
          """
             input CharacterInput {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client._
import caliban.client.__Value._

object Client {

  case class CharacterInput(name: String, nicknames: List[String] = Nil)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(
          List(
            "name"      -> implicitly[ArgEncoder[String]].encode(value.name),
            "nicknames" -> __ListValue(value.nicknames.map(value => implicitly[ArgEncoder[String]].encode(value)))
          )
        )
      override def typeName: String                       = "CharacterInput"
    }
  }

}
"""
          )
        )
      },
      testM("input object with reserved name") {
        val schema =
          """
             input CharacterInput {
               wait: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client._
import caliban.client.__Value._

object Client {

  case class CharacterInput(wait$ : String)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(List("wait" -> implicitly[ArgEncoder[String]].encode(value.wait$)))
      override def typeName: String                       = "CharacterInput"
    }
  }

}
"""
          )
        )
      },
      testM("union") {
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Captain
  object Captain {
    def shipName: SelectionBuilder[Captain, String] = Field("shipName", Scalar())
  }

  type Pilot
  object Pilot {
    def shipName: SelectionBuilder[Pilot, String] = Field("shipName", Scalar())
  }

  type Character
  object Character {
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, Option[A]] =
      Field("role", OptionOf(ChoiceOf(Map("Captain" -> Obj(onCaptain), "Pilot" -> Obj(onPilot)))))
  }

}
"""
          )
        )
      },
      testM("deprecated field + comment") {
        val schema =
          """
             type Character {
               "name"
               name: String! @deprecated(reason: "blah")
               nicknames: [String!]! @deprecated
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    /**
     * name
     */
    @deprecated("blah", "")
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    @deprecated("", "")
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("deprecated field + comment newline") {
        val tripleQuotes = "\"\"\""
        val schema       =
          """
             type Character {
               "name"
               name: String! @deprecated(reason: "foo\nbar")
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            s"""import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
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
    def name: SelectionBuilder[Character, String] = Field("name", Scalar())
  }

}
"""
          )
        )
      },
      testM("default arguments for optional and list arguments") {
        val schema =
          """
              type Query {
                characters(
                  first: Int!
                  last: Int
                  origins: [String]!
                ): String
              }""".stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._

object Client {

  type Query = RootQuery
  object Query {
    def characters(
      first: Int,
      last: Option[Int] = None,
      origins: List[Option[String]] = Nil
    ): SelectionBuilder[RootQuery, Option[String]] = Field(
      "characters",
      OptionOf(Scalar()),
      arguments = List(Argument("first", first), Argument("last", last), Argument("origins", origins))
    )
  }

}
"""
          )
        )
      },
      testM("support for Json scalar") {
        val schema =
          """
              scalar Json

              type Query {
                test: Json!
              }""".stripMargin

        assertM(gen(schema, Map("Json" -> "io.circe.Json")))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._

object Client {

  type Query = RootQuery
  object Query {
    def test: SelectionBuilder[RootQuery, io.circe.Json] = Field("test", Scalar())
  }

}
"""
          )
        )
      },
      testM("case-sensitive name uniqueness in enum's values") {
        val schema =
          """
              enum Episode {
                NEWHOPE
                EMPIRE
                JEDI
                jedi
              }
            """.stripMargin
        assertM(gen(schema))(
          equalTo(
            """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Episode extends scala.Product with scala.Serializable
  object Episode {
    case object NEWHOPE extends Episode
    case object EMPIRE  extends Episode
    case object JEDI    extends Episode
    case object jedi_1  extends Episode

    implicit val decoder: ScalarDecoder[Episode] = {
      case __StringValue("NEWHOPE") => Right(Episode.NEWHOPE)
      case __StringValue("EMPIRE")  => Right(Episode.EMPIRE)
      case __StringValue("JEDI")    => Right(Episode.JEDI)
      case __StringValue("jedi")    => Right(Episode.jedi_1)
      case other                    => Left(DecodingError(s"Can't build Episode from input $other"))
    }
    implicit val encoder: ArgEncoder[Episode]    = new ArgEncoder[Episode] {
      override def encode(value: Episode): __Value = value match {
        case Episode.NEWHOPE => __EnumValue("NEWHOPE")
        case Episode.EMPIRE  => __EnumValue("EMPIRE")
        case Episode.JEDI    => __EnumValue("JEDI")
        case Episode.jedi_1  => __EnumValue("jedi")
      }
      override def typeName: String                = "Episode"
    }
  }

}
"""
          )
        )
      },
      testM("case-insensitive name uniqueness in 2 basic objects") {
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

  type character_1
  object character_1 {
    def name: SelectionBuilder[character_1, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[character_1, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("safe names with leading and tailing _") {
        val schema =
          """
             type Character {
               _name_: String
               _nickname: String
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {
    def `_name_` : SelectionBuilder[Character, Option[String]] = Field("_name_", OptionOf(Scalar()))
    def _nickname: SelectionBuilder[Character, Option[String]] = Field("_nickname", OptionOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("add scalar mappings and additional imports") {
        val schema =
          """
             scalar OffsetDateTime

             type Order {
               date: OffsetDateTime!
             }
            """.stripMargin

        assertM(gen(schema, Map("OffsetDateTime" -> "java.time.OffsetDateTime"), List("java.util.UUID", "a.b._"))) {
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._

import java.util.UUID
import a.b._

object Client {

  type Order
  object Order {
    def date: SelectionBuilder[Order, java.time.OffsetDateTime] = Field("date", Scalar())
  }

}
"""
          )
        }
      }
    ) @@ TestAspect.sequential
}
