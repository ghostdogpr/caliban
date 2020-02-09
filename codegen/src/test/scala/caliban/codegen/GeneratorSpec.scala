package caliban.codegen

import caliban.codegen.Generator.{ Args, RootMutationDef, RootQueryDef, RootSubscriptionDef }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.ZIO
import zio.test.Assertion.equalTo
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec }

object GeneratorSpec
    extends DefaultRunnableSpec(
      suite("Generator single values")(
        testM("type with field parameter") {
          val gqltype =
            """
          type Hero {
                name(pad: Int!): String!
                nick: String!
                bday: Int
              }
              |""".stripMargin

          implicit val writer = ScalaWriter.DefaultGQLWriter

          val typeCaseClass = Parser
            .parseQuery(gqltype)
            .map(doc => {
              Document.objectTypeDefinitions(doc).map(ScalaWriter.ObjectWriter.write(_)(doc)).mkString("\n")
            })

          val typeCaseClassArgs = Parser
            .parseQuery(gqltype)
            .map(doc => {
              (for {
                typeDef      <- Document.objectTypeDefinitions(doc)
                typeDefField <- typeDef.fields
                argClass     = ScalaWriter.ArgsWriter.write(Args(typeDefField))("Hero")
                if (argClass.length > 0)
              } yield (argClass)).mkString("\n")
            })

          assertM(
            typeCaseClass,
            equalTo(
              "case class Hero(name: HeroNameArgs () => String, nick: String, bday: Option[Int])"
            )
          ) andThen assertM(
            typeCaseClassArgs,
            equalTo(
              "case class HeroNameArgs(pad: Int)"
            )
          )
        },
        testM("simple queries") {
          val query =
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
          implicit val writer = ScalaWriter.DefaultGQLWriter

          val caseclassstrdef = Parser
            .parseQuery(query)
            .map(doc => {
              Document
                .objectTypeDefinition(doc, "Query")
                .map(d => ScalaWriter.RootQueryDefWriter.write(RootQueryDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
case class UserArgs(id: Option[Int])
case class Query(
user: UserArgs => Option[User],
userList: () => List[Option[User]]
)""".stripMargin
            )
          )
        },
        testM("simple mutation") {
          val query =
            """
         type Mutation {
           setMessage(message: String): String
         }
         """
          implicit val writer = ScalaWriter.DefaultGQLWriter

          val caseclassstrdef = Parser
            .parseQuery(query)
            .map(doc => {
              Document
                .objectTypeDefinition(doc, "Mutation")
                .map(d => ScalaWriter.RootMutationDefWriter.write(RootMutationDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
                |case class SetMessageArgs(message: Option[String])
                |case class Mutation(
                |setMessage: SetMessageArgs => Option[String]
                |)""".stripMargin
            )
          )
        },
        testM("simple subscription") {
          val query =
            """
         type Subscription {
           UserWatch(id: Int!): String!
         }
         """
          implicit val writer = ScalaWriter.DefaultGQLWriter

          val caseclassstrdef = Parser
            .parseQuery(query)
            .map(doc => {
              Document
                .objectTypeDefinition(doc, "Subscription")
                .map(d => ScalaWriter.RootSubscriptionDefWriter.write(RootSubscriptionDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
                |case class UserWatchArgs(id: Int)
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

          implicit val writer = ScalaWriter.DefaultGQLWriter

          assertM(
            Parser
              .parseQuery(schema)
              .flatMap(s => {
                Generator.formatStr(Generator.generate(s), ScalaWriter.scalafmtConfig)
              }),
            equalTo(
              """import Types._
                |
                |import zio.stream.ZStream
                |
                |object Types {
                |
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
                |  case class AddPostArgs(author: Option[String], comment: Option[String])
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
          val schema = ""

          implicit val writer = ScalaWriter.DefaultGQLWriter

          assertM(
            Parser
              .parseQuery(schema)
              .flatMap(s => Generator.formatStr(Generator.generate(s), ScalaWriter.scalafmtConfig)),
            equalTo("\n")
          )
        },
        testM("enum type") {
          val gqltype =
            """
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """.stripMargin

          implicit val writer = ScalaWriter.DefaultGQLWriter

          val generated: ZIO[Any, Throwable, String] = Parser
            .parseQuery(gqltype)
            .flatMap(s => Generator.formatStr(Generator.generate(s), ScalaWriter.scalafmtConfig))

          assertM(
            generated,
            equalTo(
              """object Types {

  sealed trait Origin extends Product with Serializable

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
          val gqltype =
            """
             union Role = Captain | Pilot
             
             type Captain {
               shipName: String!
             }
             
             type Pilot {
               shipName: String!
             }
            """.stripMargin

          implicit val writer = ScalaWriter.DefaultGQLWriter

          val generated: ZIO[Any, Throwable, String] = Parser
            .parseQuery(gqltype)
            .flatMap(s => Generator.formatStr(Generator.generate(s), ScalaWriter.scalafmtConfig))

          assertM(
            generated,
            equalTo(
              """object Types {

  sealed trait Role extends Product with Serializable

  object Role {
    case class Captain(shipName: String) extends Role
    case class Pilot(shipName: String)   extends Role
  }

}
"""
            )
          )
        }
      )
    )
