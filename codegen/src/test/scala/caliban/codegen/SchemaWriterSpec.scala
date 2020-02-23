package caliban.codegen

import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.Task
import zio.test.Assertion.equalTo
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec }

object SchemaWriterSpec
    extends DefaultRunnableSpec({

      val gen: String => Task[String] = (schema: String) =>
        Parser
          .parseQuery(schema)
          .flatMap(doc => Formatter.format(SchemaWriter.write(doc), None))

      suite("Generator single values")(
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
            .map(doc => Document.objectTypeDefinitions(doc).map(SchemaWriter.writeObject).mkString("\n"))

          val typeCaseClassArgs = Parser
            .parseQuery(schema)
            .map { doc =>
              (for {
                typeDef      <- Document.objectTypeDefinitions(doc)
                typeDefField <- typeDef.fields
                argClass     = SchemaWriter.writeArguments(typeDefField) if argClass.length > 0
              } yield argClass).mkString("\n")
            }

          assertM(
            typeCaseClass,
            equalTo(
              "case class Hero(name: NameArgs () => String, nick: String, bday: Option[Int])"
            )
          ) andThen assertM(
            typeCaseClassArgs,
            equalTo(
              "case class NameArgs(pad: Int)"
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
            .map { doc =>
              Document
                .objectTypeDefinition(doc, "Query")
                .map(SchemaWriter.writeRootQueryOrMutationDef)
                .mkString("\n")
            }

          assertM(
            result,
            equalTo(
              """
case class Query(
user: UserArgs => Option[User],
userList: () => List[Option[User]]
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
            .map { doc =>
              Document
                .objectTypeDefinition(doc, "Mutation")
                .map(SchemaWriter.writeRootQueryOrMutationDef)
                .mkString("\n")
            }

          assertM(
            result,
            equalTo(
              """
                |case class Mutation(
                |setMessage: SetMessageArgs => Option[String]
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
            .map { doc =>
              Document
                .objectTypeDefinition(doc, "Subscription")
                .map(SchemaWriter.writeRootSubscriptionDef)
                .mkString("\n")
            }

          assertM(
            result,
            equalTo(
              """
                |case class Subscription(
                |UserWatch: UserWatchArgs => ZStream[Any, Nothing, String]
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

          assertM(
            gen(schema),
            equalTo(
              """import Types._
                |
                |import zio.stream.ZStream
                |
                |object Types {
                |  case class AddPostArgs(author: Option[String], comment: Option[String])
                |  case class Post(author: Option[String], comment: Option[String])
                |
                |}
                |
                |object Operations {
                |
                |  case class Query(
                |    posts: () => Option[List[Option[Post]]]
                |  )
                |
                |  case class Mutation(
                |    addPost: AddPostArgs => Option[Post]
                |  )
                |
                |  case class Subscription(
                |    postAdded: () => ZStream[Any, Nothing, Option[Post]]
                |  )
                |
                |}
                |""".stripMargin
            )
          )
        },
        testM("empty schema test") {
          assertM(gen(""), equalTo("\n"))
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

          assertM(
            gen(schema),
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
          val schema =
            """
             "role"
             union Role = Captain | Pilot
             
             type Captain {
               "ship" shipName: String!
             }
             
             type Pilot {
               shipName: String!
             }
            """.stripMargin

          assertM(
            gen(schema),
            equalTo(
              """import caliban.schema.Annotations._

object Types {

  @GQLDescription("role")
  sealed trait Role extends scala.Product with scala.Serializable

  object Role {
    case class Captain(
      @GQLDescription("ship")
      shipName: String
    ) extends Role
    case class Pilot(shipName: String) extends Role
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
               query: Queries
             }
               
             type Queries {
               characters: Int!
             }
            """.stripMargin

          assertM(
            gen(schema),
            equalTo(
              """object Operations {

  case class Queries(
    characters: () => Int
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

          assertM(
            gen(schema),
            equalTo(
              """object Types {

  case class Character(name: String)
  case class CharacterArgs(name: String)

}
"""
            )
          )
        }
      )
    })
