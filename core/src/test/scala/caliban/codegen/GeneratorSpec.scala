package caliban.codegen
import caliban.codegen.Generator.{ Args, RootMutationDef, RootQueryDef, RootSubscriptionDef }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.test.Assertion._
import zio.test._
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
              Document.typeDefinitions(doc).map(ScalaWriter.TypeDefinitionWriter.write(_)(doc)).mkString("\n")
            })

          val typeCaseClassArgs = Parser
            .parseQuery(gqltype)
            .map(doc => {
              (for {
                typeDef      <- Document.typeDefinitions(doc)
                typeDefField <- typeDef.children
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
        testM("simple fragment") {
          val query =
            """
         type User {
           id: Int
           name: String
           profilePic: String
         }
         fragment friendFields on User {
           id
           name
           profilePic(size: 50)
         }"""

          implicit val writer = ScalaWriter.DefaultGQLWriter

          val caseclassstrdef = Parser
            .parseQuery(query)
            .map(doc => {
              Document.fragmentDefinitions(doc).map(d => ScalaWriter.FragmentWriter.write(d)(doc)).mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """case class FriendFields(id: Option[Int], name: Option[String], profilePic: Option[String])""".stripMargin
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
                .typeDefinition("Query")(doc)
                .map(d => ScalaWriter.RootQueryDefWriter.write(RootQueryDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
case class UserArgs(id: Option[Int])
case class Queries(
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
                .typeDefinition("Mutation")(doc)
                .map(d => ScalaWriter.RootMutationDefWriter.write(RootMutationDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
                |case class SetMessageArgs(message: Option[String])
                |case class Mutations(
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
                .typeDefinition("Subscription")(doc)
                .map(d => ScalaWriter.RootSubscriptionDefWriter.write(RootSubscriptionDef(d))(doc))
                .mkString("\n")
            })

          assertM(
            caseclassstrdef,
            equalTo(
              """
                |case class UserWatchArgs(id: Int)
                |case class Subscriptions(
                |UserWatch: UserWatchArgs => ZStream[Console, Nothing, String]
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
              |  fragment PostAuthor on Post {
              |    author
              |  }
              |""".stripMargin

          implicit val writer = ScalaWriter.DefaultGQLWriter

          assertM(
            Parser.parseQuery(schema).flatMap(s => Generator.format(Generator.generate(s), ".scalafmt.conf")),
            equalTo("""import Types._
                      |import Fragments._
                      |import zio.console.Console
                      |import zio.stream.ZStream
                      |
                      |object Types {
                      |
                      |  case class Post(author: Option[String], comment: Option[String])
                      |}
                      |
                      |object Operations {
                      |
                      |  case class Queries(
                      |    posts: () => Option[List[Option[Post]]]
                      |  )
                      |
                      |  case class AddPostArgs(author: Option[String], comment: Option[String])
                      |  case class Mutations(
                      |    addPost: AddPostArgs => Option[Post]
                      |  )
                      |
                      |  case class Subscriptions(
                      |    postAdded: () => ZStream[Console, Nothing, Option[Post]]
                      |  )
                      |}
                      |
                      |object Fragments {
                      |  case class PostAuthor(author: Option[String])
                      |}
                      |""".stripMargin)
          )
        }
      )
    )
