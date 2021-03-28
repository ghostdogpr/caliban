package caliban.tools

import caliban.parsing.Parser
import caliban.tools.implicits.ScalarMappings
import zio.Task
import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment

object SchemaWriterSpec extends DefaultRunnableSpec {

  implicit val scalarMappings: ScalarMappings = ScalarMappings(None)

  def gen(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty,
    customImports: List[String] = List.empty
  ): Task[String] = Parser
    .parseQuery(schema)
    .flatMap(doc =>
      Formatter
        .format(SchemaWriter.write(doc, imports = Some(customImports))(ScalarMappings(Some(scalarMappings))), None)
    )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SchemaWriterSpec")(
      testM("type with field parameter") {
        val schema =
          """
          type Hero {
                name(pad: Int!): String!
                nick: String!
                bday: Int
              }
            |""".stripMargin

        val typeCaseClass = Parser
          .parseQuery(schema)
          .map(_.objectTypeDefinitions.map(SchemaWriter.writeObject).mkString("\n"))
          .flatMap(Formatter.format(_, None).map(_.trim))

        val typeCaseClassArgs = Parser
          .parseQuery(schema)
          .map { doc =>
            (for {
              typeDef      <- doc.objectTypeDefinitions
              typeDefField <- typeDef.fields
              argClass      = SchemaWriter.writeArguments(typeDefField, typeDef) if argClass.length > 0
            } yield argClass).mkString("\n")
          }
          .flatMap(Formatter.format(_, None).map(_.trim))

        assertM(typeCaseClass)(
          equalTo(
            "case class Hero(name: HeroNameArgs () => String, nick: String, bday: Option[Int])"
          )
        ) andThen assertM(typeCaseClassArgs)(
          equalTo(
            "case class HeroNameArgs(pad: Int)"
          )
        )
      },
      testM("simple queries") {
        val schema =
          """
         type Query {
           user(id: Int): User
           userList: [User]!
         }
         type User {
           id: Int
           name: String
           profilePic: String
         }"""

        val result = Parser
          .parseQuery(schema)
          .map(
            _.objectTypeDefinition("Query").map(SchemaWriter.writeRootQueryOrMutationDef(_, "zio.UIO")).mkString("\n")
          )
          .flatMap(Formatter.format(_, None).map(_.trim))

        assertM(result)(
          equalTo(
            """case class Query(
  user: QueryUserArgs => zio.UIO[Option[User]],
  userList: zio.UIO[List[Option[User]]]
)""".stripMargin
          )
        )
      },
      testM("simple mutation") {
        val schema =
          """
         type Mutation {
           setMessage(message: String): String
         }
         """
        val result = Parser
          .parseQuery(schema)
          .map(
            _.objectTypeDefinition("Mutation")
              .map(SchemaWriter.writeRootQueryOrMutationDef(_, "zio.UIO"))
              .mkString("\n")
          )
          .flatMap(Formatter.format(_, None).map(_.trim))

        assertM(result)(
          equalTo(
            """case class Mutation(
              |  setMessage: MutationSetMessageArgs => zio.UIO[Option[String]]
              |)""".stripMargin
          )
        )
      },
      testM("simple subscription") {
        val schema =
          """
         type Subscription {
           UserWatch(id: Int!): String!
         }
         """

        val result = Parser
          .parseQuery(schema)
          .map(_.objectTypeDefinition("Subscription").map(SchemaWriter.writeRootSubscriptionDef).mkString("\n"))

        assertM(result)(
          equalTo(
            """
              |case class Subscription(
              |UserWatch: SubscriptionUserWatchArgs => ZStream[Any, Nothing, String]
              |)""".stripMargin
          )
        )
      },
      testM("schema test") {
        val schema =
          """
            |  type Subscription {
            |    postAdded: Post
            |  }
            |  type Query {
            |    posts: [Post]
            |  }
            |  type Mutation {
            |    addPost(author: String, comment: String): Post
            |  }
            |  type Post {
            |    author: String
            |    comment: String
            |  }
            |""".stripMargin

        assertM(gen(schema))(
          equalTo(
            """import Types._
              |
              |import zio.stream.ZStream
              |
              |object Types {
              |  case class MutationAddPostArgs(author: Option[String], comment: Option[String])
              |  case class Post(author: Option[String], comment: Option[String])
              |
              |}
              |
              |object Operations {
              |
              |  case class Query(
              |    posts: zio.UIO[Option[List[Option[Post]]]]
              |  )
              |
              |  case class Mutation(
              |    addPost: MutationAddPostArgs => zio.UIO[Option[Post]]
              |  )
              |
              |  case class Subscription(
              |    postAdded: ZStream[Any, Nothing, Option[Post]]
              |  )
              |
              |}
              |""".stripMargin
          )
        )
      },
      testM("empty schema test") {
        assertM(gen(""))(equalTo(System.lineSeparator))
      },
      testM("enum type") {
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
            """object Types {

  sealed trait Origin extends scala.Product with scala.Serializable

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
  }

}
"""
          )
        )
      },
      testM("union type") {
        val role   =
          s"""
              \"\"\"
             role
             Captain or Pilot
             \"\"\"
          """
        val schema =
          s"""
             $role
             union Role = Captain | Pilot
             
             type Captain {
               "ship" shipName: String!
             }
             
             type Pilot {
               shipName: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo {
            val role =
              s"""\"\"\"role
Captain or Pilot\"\"\""""
            s"""import caliban.schema.Annotations._

object Types {

  @GQLDescription($role)
  sealed trait Role extends scala.Product with scala.Serializable

  object Role {
    case class Captain(
      @GQLDescription("ship")
      shipName: String
    )                                  extends Role
    case class Pilot(shipName: String) extends Role
  }

}
"""
          }
        )
      },
      testM("GQLDescription with escaped quotes") {
        val schema =
          s"""
             type Captain {
               "foo \\"quotes\\" bar" shipName: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo {
            s"""import caliban.schema.Annotations._

object Types {

  case class Captain(
    @GQLDescription("foo \\"quotes\\" bar")
    shipName: String
  )

}
"""
          }
        )
      },
      testM("schema") {
        val schema =
          """
             schema {
               query: Queries
             }
               
             type Queries {
               characters: Int!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """object Operations {

  case class Queries(
    characters: zio.UIO[Int]
  )

}
"""
          )
        )
      },
      testM("input type") {
        val schema =
          """
             type Character {
                name: String!
             }
              
             input CharacterArgs {
               name: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """object Types {

  case class Character(name: String)
  case class CharacterArgs(name: String)

}
"""
          )
        )
      },
      testM("scala reserved word used") {
        val schema =
          """
             type Character {
               private: String!
               object: String!
               type: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """object Types {

  case class Character(`private`: String, `object`: String, `type`: String)

}
"""
          )
        )
      },
      testM("case class reserved field name used") {
        val schema =
          """
             type Character {
               wait: String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """object Types {

  case class Character(wait$ : String)

}
"""
          )
        )
      },
      testM("args unique class names") {
        val schema =
          """
            |type Hero {
            |  callAllies(number: Int!): [Hero!]!
            |}
            |
            |type Villain {
            |  callAllies(number: Int!, w: String!): [Villain!]!
            |}
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """object Types {
              |  case class HeroCallAlliesArgs(number: Int)
              |  case class VillainCallAlliesArgs(number: Int, w: String)
              |  case class Hero(callAllies: HeroCallAlliesArgs => List[Hero])
              |  case class Villain(callAllies: VillainCallAlliesArgs => List[Villain])
              |
              |}
              |""".stripMargin
          )
        )
      },
      testM("args names root level") {
        val schema =
          """
            |schema {
            |  query: Query
            |  subscription: Subscription
            |}
            |
            |type Params {
            |  p: Int!
            |}
            |
            |type Query {
            |  characters(p: Params!): Int!
            |}
            |
            |type Subscription {
            |  characters(p: Params!): Int!
            |}
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import Types._
              |
              |import zio.stream.ZStream
              |
              |object Types {
              |  case class QueryCharactersArgs(p: Params)
              |  case class SubscriptionCharactersArgs(p: Params)
              |  case class Params(p: Int)
              |
              |}
              |
              |object Operations {
              |
              |  case class Query(
              |    characters: QueryCharactersArgs => zio.UIO[Int]
              |  )
              |
              |  case class Subscription(
              |    characters: SubscriptionCharactersArgs => ZStream[Any, Nothing, Int]
              |  )
              |
              |}
              |""".stripMargin
          )
        )
      },
      testM("add scalar mappings and additional imports") {
        val schema =
          """
            |  scalar OffsetDateTime
            |
            |  type Subscription {
            |    postAdded: Post
            |  }
            |  type Query {
            |    posts: [Post]
            |  }
            |  type Mutation {
            |    addPost(author: String, comment: String): Post
            |  }
            |  type Post {
            |    date: OffsetDateTime!
            |    author: String
            |    comment: String
            |  }
            |""".stripMargin

        assertM(gen(schema, Map("OffsetDateTime" -> "java.time.OffsetDateTime"), List("java.util.UUID", "a.b._")))(
          equalTo(
            """import Types._
              |
              |import zio.stream.ZStream
              |
              |import java.util.UUID
              |import a.b._
              |
              |object Types {
              |  case class MutationAddPostArgs(author: Option[String], comment: Option[String])
              |  case class Post(date: java.time.OffsetDateTime, author: Option[String], comment: Option[String])
              |
              |}
              |
              |object Operations {
              |
              |  case class Query(
              |    posts: zio.UIO[Option[List[Option[Post]]]]
              |  )
              |
              |  case class Mutation(
              |    addPost: MutationAddPostArgs => zio.UIO[Option[Post]]
              |  )
              |
              |  case class Subscription(
              |    postAdded: ZStream[Any, Nothing, Option[Post]]
              |  )
              |
              |}
              |""".stripMargin
          )
        )
      }
    ) @@ TestAspect.sequential
}
