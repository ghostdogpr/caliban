package caliban.tools

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion._
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec, TestAspect, ZSpec }
import zio.test.environment.TestEnvironment

object ClientWriterSpec extends DefaultRunnableSpec {

  val gen: String => Task[String] = (schema: String) =>
    Parser
      .parseQuery(schema)
      .flatMap(doc => Formatter.format(ClientWriter.write(doc), None))

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
    implicit val encoder: ArgEncoder[Origin] = new ArgEncoder[Origin] {
      override def encode(value: Origin): __Value = value match {
        case Origin.EARTH => __EnumValue("EARTH")
        case Origin.MARS  => __EnumValue("MARS")
        case Origin.BELT  => __EnumValue("BELT")
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
      override def typeName: String = "CharacterInput"
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

  case class CharacterInput(wait_ : String)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(List("wait" -> implicitly[ArgEncoder[String]].encode(value.wait_)))
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
    def name: SelectionBuilder[Character, String] = Field("name", Scalar())
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
        val schema =
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
    @deprecated(${tripleQuotes}foo\nbar$tripleQuotes, "")
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
    ): SelectionBuilder[RootQuery, Option[String]] =
      Field(
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

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._

object Client {

  type Json = io.circe.Json

  type Query = RootQuery
  object Query {
    def test: SelectionBuilder[RootQuery, Json] = Field("test", Scalar())
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
import caliban.client.Value._

object Client {

  sealed trait Episode extends scala.Product with scala.Serializable
  object Episode {
    case object NEWHOPE extends Episode
    case object EMPIRE  extends Episode
    case object JEDI    extends Episode
    case object jedi_   extends Episode

    implicit val decoder: ScalarDecoder[Episode] = {
      case StringValue("NEWHOPE") => Right(Episode.NEWHOPE)
      case StringValue("EMPIRE")  => Right(Episode.EMPIRE)
      case StringValue("JEDI")    => Right(Episode.JEDI)
      case StringValue("jedi")    => Right(Episode.jedi_)
      case other                  => Left(DecodingError(s"Can't build Episode from input $other"))
    }
    implicit val encoder: ArgEncoder[Episode] = new ArgEncoder[Episode] {
      override def encode(value: Episode): __Value = value match {
        case Episode.NEWHOPE => EnumValue("NEWHOPE")
        case Episode.EMPIRE  => EnumValue("EMPIRE")
        case Episode.JEDI    => EnumValue("JEDI")
        case Episode.jedi_   => EnumValue("jedi")
      }
      override def typeName: String = "Episode"
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

  type character_
  object character_ {
    def name: SelectionBuilder[character_, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[character_, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("case-insensitive name uniqueness in emuns, input objects, interfaces and union types") {
        val schema =
          """
          interface Character {
            name: String!
          }

          interface character {
            name: String!
          }

          enum Episode {
            NEWHOPE
            EMPIRE
            JEDI
            jedi
          }

          input MessageInput {
              content: String
              author: String
            }
          
          input messageInput {
              content: String
              author: String
          }

          input MessageInpuT {
              content: String
              author: String
          }

          type Human implements Character {
            name:String!
          }

          type human implements character {
            name:String!
          }

          type humaN implements character {
            name:String!
          }

          type Droid {
            name:Human!
          }

          type Starship {
            name:human!
          }

          union SearchResult = Human | human
          union searchResult = Human | human

          type Mutation {
            createMessage(input: messageInput): String!
            createMessage2(input: messageInput): human!
            createMessage3(input: messageInput): Human!
            createMessage4(input: MessageInpuT): humaN!
            createMessage5(input: MessageInpuT): Character!
            createMessage6(input: MessageInpuT): character!
            createMessage7(input: MessageInpuT): SearchResult!
            createMessage8(input: MessageInpuT): searchResult!
          }

          type Query {
            episode(tpe: Episode = jedi): human!
            episode2(tpe: Episode = JEDI): Human
            episode3(tpe: Episode = JEDI): humaN
            episode4(tpe: Episode = JEDI): Character!
            episode5(tpe: Episode = JEDI): character!
            episode6(tpe: Episode = JEDI): SearchResult!
            episode7(tpe: Episode = JEDI): searchResult!
          }

        """.stripMargin
        assertM(gen(schema))(
          equalTo(
            """import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._
import caliban.client.Value._

object Client {

  sealed trait Episode extends scala.Product with scala.Serializable
  object Episode {
    case object NEWHOPE extends Episode
    case object EMPIRE  extends Episode
    case object JEDI    extends Episode
    case object jedi_   extends Episode

    implicit val decoder: ScalarDecoder[Episode] = {
      case StringValue("NEWHOPE") => Right(Episode.NEWHOPE)
      case StringValue("EMPIRE")  => Right(Episode.EMPIRE)
      case StringValue("JEDI")    => Right(Episode.JEDI)
      case StringValue("jedi")    => Right(Episode.jedi_)
      case other                  => Left(DecodingError(s"Can't build Episode from input $other"))
    }
    implicit val encoder: ArgEncoder[Episode] = new ArgEncoder[Episode] {
      override def encode(value: Episode): __Value = value match {
        case Episode.NEWHOPE => EnumValue("NEWHOPE")
        case Episode.EMPIRE  => EnumValue("EMPIRE")
        case Episode.JEDI    => EnumValue("JEDI")
        case Episode.jedi_   => EnumValue("jedi")
      }
      override def typeName: String = "Episode"
    }
  }

  type Human
  object Human {
    def name: SelectionBuilder[Human, String] = Field("name", Scalar())
  }

  type human__
  object human__ {
    def name: SelectionBuilder[human__, String] = Field("name", Scalar())
  }

  type humaN_
  object humaN_ {
    def name: SelectionBuilder[humaN_, String] = Field("name", Scalar())
  }

  type Droid
  object Droid {
    def name[A](innerSelection: SelectionBuilder[Human, A]): SelectionBuilder[Droid, A] =
      Field("name", Obj(innerSelection))
  }

  type Starship
  object Starship {
    def name[A](innerSelection: SelectionBuilder[human__, A]): SelectionBuilder[Starship, A] =
      Field("name", Obj(innerSelection))
  }

  case class MessageInput_(content: Option[String] = None, author: Option[String] = None)
  object MessageInput_ {
    implicit val encoder: ArgEncoder[MessageInput_] = new ArgEncoder[MessageInput_] {
      override def encode(value: MessageInput_): __Value =
        __ObjectValue(
          List(
            "content" -> value.content.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value)),
            "author"  -> value.author.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value))
          )
        )
      override def typeName: String = "MessageInput_"
    }
  }
  case class messageInput__(content: Option[String] = None, author: Option[String] = None)
  object messageInput__ {
    implicit val encoder: ArgEncoder[messageInput__] = new ArgEncoder[messageInput__] {
      override def encode(value: messageInput__): __Value =
        __ObjectValue(
          List(
            "content" -> value.content.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value)),
            "author"  -> value.author.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value))
          )
        )
      override def typeName: String = "messageInput__"
    }
  }
  case class MessageInpuT(content: Option[String] = None, author: Option[String] = None)
  object MessageInpuT {
    implicit val encoder: ArgEncoder[MessageInpuT] = new ArgEncoder[MessageInpuT] {
      override def encode(value: MessageInpuT): __Value =
        __ObjectValue(
          List(
            "content" -> value.content.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value)),
            "author"  -> value.author.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value))
          )
        )
      override def typeName: String = "MessageInpuT"
    }
  }
  type Query = RootQuery
  object Query {
    def episode[A](tpe: Option[Episode] = None)(
      innerSelection: SelectionBuilder[human__, A]
    ): SelectionBuilder[RootQuery, A] = Field("episode", Obj(innerSelection), arguments = List(Argument("tpe", tpe)))
    def episode2[A](
      tpe: Option[Episode] = None
    )(innerSelection: SelectionBuilder[Human, A]): SelectionBuilder[RootQuery, Option[A]] =
      Field("episode2", OptionOf(Obj(innerSelection)), arguments = List(Argument("tpe", tpe)))
    def episode3[A](
      tpe: Option[Episode] = None
    )(innerSelection: SelectionBuilder[humaN_, A]): SelectionBuilder[RootQuery, Option[A]] =
      Field("episode3", OptionOf(Obj(innerSelection)), arguments = List(Argument("tpe", tpe)))
    def episode4[A](
      tpe: Option[Episode] = None
    )(onHuman: Option[SelectionBuilder[Human, A]] = None): SelectionBuilder[RootQuery, A] =
      Field(
        "episode4",
        ChoiceOf(Map("Human" -> onHuman).collect { case (k, Some(v)) => k -> Obj(v) }),
        arguments = List(Argument("tpe", tpe))
      )
    def episode5[A](tpe: Option[Episode] = None)(
      onhumaN: Option[SelectionBuilder[humaN_, A]] = None,
      onhuman: Option[SelectionBuilder[human__, A]] = None
    ): SelectionBuilder[RootQuery, A] =
      Field("episode5", ChoiceOf(Map("humaN" -> onhumaN, "human" -> onhuman).collect {
        case (k, Some(v)) => k -> Obj(v)
      }), arguments = List(Argument("tpe", tpe)))
    def episode6[A](
      tpe: Option[Episode] = None
    )(onHuman: SelectionBuilder[Human, A], onhuman: SelectionBuilder[human__, A]): SelectionBuilder[RootQuery, A] =
      Field(
        "episode6",
        ChoiceOf(Map("Human" -> Obj(onHuman), "human" -> Obj(onhuman))),
        arguments = List(Argument("tpe", tpe))
      )
    def episode7[A](
      tpe: Option[Episode] = None
    )(onHuman: SelectionBuilder[Human, A], onhuman: SelectionBuilder[human__, A]): SelectionBuilder[RootQuery, A] =
      Field(
        "episode7",
        ChoiceOf(Map("Human" -> Obj(onHuman), "human" -> Obj(onhuman))),
        arguments = List(Argument("tpe", tpe))
      )
  }

  type Mutation = RootMutation
  object Mutation {
    def createMessage(input: Option[messageInput__] = None): SelectionBuilder[RootMutation, String] =
      Field("createMessage", Scalar(), arguments = List(Argument("input", input)))
    def createMessage2[A](
      input: Option[messageInput__] = None
    )(innerSelection: SelectionBuilder[human__, A]): SelectionBuilder[RootMutation, A] =
      Field("createMessage2", Obj(innerSelection), arguments = List(Argument("input", input)))
    def createMessage3[A](
      input: Option[messageInput__] = None
    )(innerSelection: SelectionBuilder[Human, A]): SelectionBuilder[RootMutation, A] =
      Field("createMessage3", Obj(innerSelection), arguments = List(Argument("input", input)))
    def createMessage4[A](
      input: Option[MessageInpuT] = None
    )(innerSelection: SelectionBuilder[humaN_, A]): SelectionBuilder[RootMutation, A] =
      Field("createMessage4", Obj(innerSelection), arguments = List(Argument("input", input)))
    def createMessage5[A](
      input: Option[MessageInpuT] = None
    )(onHuman: Option[SelectionBuilder[Human, A]] = None): SelectionBuilder[RootMutation, A] =
      Field(
        "createMessage5",
        ChoiceOf(Map("Human" -> onHuman).collect { case (k, Some(v)) => k -> Obj(v) }),
        arguments = List(Argument("input", input))
      )
    def createMessage6[A](input: Option[MessageInpuT] = None)(
      onhumaN: Option[SelectionBuilder[humaN_, A]] = None,
      onhuman: Option[SelectionBuilder[human__, A]] = None
    ): SelectionBuilder[RootMutation, A] =
      Field("createMessage6", ChoiceOf(Map("humaN" -> onhumaN, "human" -> onhuman).collect {
        case (k, Some(v)) => k -> Obj(v)
      }), arguments = List(Argument("input", input)))
    def createMessage7[A](
      input: Option[MessageInpuT] = None
    )(onHuman: SelectionBuilder[Human, A], onhuman: SelectionBuilder[human__, A]): SelectionBuilder[RootMutation, A] =
      Field(
        "createMessage7",
        ChoiceOf(Map("Human" -> Obj(onHuman), "human" -> Obj(onhuman))),
        arguments = List(Argument("input", input))
      )
    def createMessage8[A](
      input: Option[MessageInpuT] = None
    )(onHuman: SelectionBuilder[Human, A], onhuman: SelectionBuilder[human__, A]): SelectionBuilder[RootMutation, A] =
      Field(
        "createMessage8",
        ChoiceOf(Map("Human" -> Obj(onHuman), "human" -> Obj(onhuman))),
        arguments = List(Argument("input", input))
      )
  }

}
"""
          )
        )
      }
    ) @@ TestAspect.sequential
}
