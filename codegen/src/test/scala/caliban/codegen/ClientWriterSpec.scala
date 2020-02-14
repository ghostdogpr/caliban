package caliban.codegen

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion._
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec }

object ClientWriterSpec
    extends DefaultRunnableSpec(
      {
        val gen: String => Task[String] = (schema: String) =>
          Parser
            .parseQuery(schema)
            .flatMap(doc => Formatter.formatStr(ClientWriter.write(doc), CodegenPlugin.scalafmtConfig))

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
                """object Data {

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
          testM("nested object type") {
            val schema =
              """
             type Query {
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
                """object Data {

  type Query
  object Query {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Query, List[A]] =
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
             type Query {
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
                """object Data {

  type Query
  object Query {
    def character[A](name: String)(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Query, Option[A]] =
      Field("character", OptionOf(Obj(innerSelection)))
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
               query: Query
             }
             
             type Query {
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
                """object Data {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

  object Query {
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
                """object Data {

  sealed trait Origin extends Product with Serializable
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
        case EARTH => StringValue("EARTH")
        case MARS  => StringValue("MARS")
        case BELT  => StringValue("BELT")
      }
      override def typeName: String = "Origin"
    }
  }

}
"""
              )
            )
          }
        )
      }
    )
