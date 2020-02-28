package caliban.codegen

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion._
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec, TestAspect }

object ClientWriterSpec
    extends DefaultRunnableSpec(
      {
        val gen: String => Task[String] = (schema: String) =>
          Parser
            .parseQuery(schema)
            .flatMap(doc => Formatter.format(ClientWriter.write(doc), None))

        suite("ClientWriterSpec")(
          testM("simple object type") {
            val schema =
              """
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
              equalTo(
                """import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable
  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin

    implicit val decoder: ScalarDecoder[Origin] = {
      case StringValue("EARTH") => Right(Origin.EARTH)
      case StringValue("MARS")  => Right(Origin.MARS)
      case StringValue("BELT")  => Right(Origin.BELT)
      case other                => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin] = new ArgEncoder[Origin] {
      override def encode(value: Origin): Value = value match {
        case Origin.EARTH => StringValue("EARTH")
        case Origin.MARS  => StringValue("MARS")
        case Origin.BELT  => StringValue("BELT")
      }
      override def typeName: String = "Origin"
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

            assertM(
              gen(schema),
              equalTo(
                """import caliban.client._
import caliban.client.Value._

object Client {

  case class CharacterInput(name: String, nicknames: List[String])
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): Value =
        ObjectValue(
          List(
            "name"      -> implicitly[ArgEncoder[String]].encode(value.name),
            "nicknames" -> ListValue(value.nicknames.map(value => implicitly[ArgEncoder[String]].encode(value)))
          )
        )
      override def typeName: String = "CharacterInput"
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

            assertM(
              gen(schema),
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

            assertM(
              gen(schema),
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
    def name: SelectionBuilder[Character, String] = Field("name", Scalar())
    @deprecated("", "")
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
              )
            )
          }
        ) @@ TestAspect.sequential
      }
    )
