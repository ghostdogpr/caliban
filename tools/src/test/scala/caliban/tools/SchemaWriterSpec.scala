package caliban.tools

import caliban.parsing.Parser
import zio.Task
import zio.test.environment.TestEnvironment
import zio.test._

object SchemaWriterSpec extends DefaultRunnableSpec {

  val gen: String => Task[String] = (schema: String) =>
    Parser
      .parseQuery(schema)
      .flatMap(doc =>
        Formatter.format(
          SchemaWriter.write(
            schema = doc,
            objectName = "ObjectName",
            packageName = Some("caliban.tools.testing"),
            effect = "caliban.CalibanEffect",
            typeMappings = Map("NonEmptyString" -> "eu.timepit.refined.types.string.NonEmptyString")
          ),
          None
        )
      )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SchemaWriterSpec")(
      testM("type with field parameter with type mapping") {
        val schema =
          """
            |scalar Name
            |
            |type Hero {
            |  name(pad: Int!): Name!
            |  nick: NonEmptyString!
            |  bday: Int
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |object Types {
              |  case class NameArgs(pad: Int)
              |  case class Hero(name: HeroNameArgs => Name, nick: eu.timepit.refined.types.string.NonEmptyString, bday: Option[Int])
              |}
              |""".stripMargin
          )
        )
      },
      testM("simple queries") {
        val schema =
          """
            |type Query {
            |  user(id: Int): User
            |  userList: [User]!
            |}
            |
            |type User {
            |  id: Int
            |  name: String
            |  profilePic: String
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |import Types._
              |
              |object Types {
              |  case class UserArgs(id: Option[Int])
              |  case class User(id: Option[Int], name: Option[String], profilePic: Option[String])
              |}
              |
              |object Operations {
              |  case class Query(
              |    user: UserArgs => caliban.CalibanEffect[Option[User]],
              |    userList: caliban.CalibanEffect[List[Option[User]]]
              |  )
              |}
              |""".stripMargin
          )
        )
      },
      testM("simple mutation") {
        val schema =
          """
            |type Mutation {
            |  setMessage(message: String): String
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |import Types._
              |
              |object Types {
              |  case class SetMessageArgs(message: Option[String])
              |}
              |
              |object Operations {
              |  case class Mutation(
              |    setMessage: SetMessageArgs => caliban.CalibanEffect[Option[String]]
              |  )
              |}
              |
              |""".stripMargin
          )
        )
      },
      testM("simple subscription") {
        val schema =
          """
            |type Subscription {
            |  UserWatch(id: Int!): String!
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |import Types._
              |
              |import zio.stream.ZStream
              |
              |object Types {
              |  case class UserWatchArgs(id: Int)
              |}
              |
              |object Operations {
              |  case class Subscription(
              |    UserWatch: UserWatchArgs => ZStream[Any, Nothing, String]
              |  )
              |}
              |""".stripMargin
          )
        )
      },
      testM("schema test") {
        val schema =
          """
            |type Subscription {
            |  postAdded: Post
            |}
            |
            |type Query {
            |  posts: [Post]
            |}
            |
            |type Mutation {
            |  addPost(author: String, comment: String): Post
            |}
            |
            |type Post {
            |  author: String
            |  comment: String
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |import Types._
              |
              |import zio.stream.ZStream
              |
              |object Types {
              |  case class AddPostArgs(author: Option[String], comment: Option[String])
              |  case class Post(author: Option[String], comment: Option[String])
              |}
              |
              |object Operations {
              |
              |  case class Query(
              |    posts: caliban.CalibanEffect[Option[List[Option[Post]]]]
              |  )
              |
              |  case class Mutation(
              |    addPost: AddPostArgs => caliban.CalibanEffect[Option[Post]]
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
        assertM(gen(""))(stringEqualTo("package caliban.tools.testing"))
      },
      testM("enum type") {
        val schema =
          """
            |enum Origin {
            |  EARTH
            |  MARS
            |  BELT
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |object Types {
              |
              |  sealed trait Origin extends scala.Product with scala.Serializable
              |
              |  object Origin {
              |    case object EARTH extends Origin
              |    case object MARS  extends Origin
              |    case object BELT  extends Origin
              |  }
              |}
              |""".stripMargin
          )
        )
      },
      testM("union type") {
        val role =
          s"""
             |\"\"\"
             |role
             |Captain or Pilot
             |\"\"\"
          """.stripMargin

        val schema =
          s"""
             |$role
             |union Role = Captain | Pilot
             |
             |type Captain {
             |  "ship" shipName: String!
             |}
             |
             |type Pilot {
             |  shipName: String!
             |}
             |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo {
            val role =
              s"""\"\"\"role
                 |Captain or Pilot\"\"\"""".stripMargin

            s"""
               |package caliban.tools.testing
               |
               |import caliban.schema.Annotations._
               |
               |object Types {
               |
               |  @GQLDescription($role)
               |  sealed trait Role extends scala.Product with scala.Serializable
               |
               |  object Role {
               |    case class Captain(
               |      @GQLDescription("ship")
               |      shipName: String
               |    ) extends Role
               |
               |    case class Pilot(shipName: String) extends Role
               |  }
               |
               |}
               |""".stripMargin
          }
        )
      },
      testM("schema") {
        val schema =
          """
            |schema {
            |  query: Queries
            |}
            |
            |type Queries {
            |  characters: Int!
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |object Operations {
              |  case class Queries(
              |    characters: caliban.CalibanEffect[Int]
              |  )
              |}
              |""".stripMargin
          )
        )
      },
      testM("input type") {
        val schema =
          """
            |type Character {
            |  name: String!
            |}
            |
            |input CharacterArgs {
            |  name: String!
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |object Types {
              |  case class Character(name: String)
              |  case class CharacterArgs(name: String)
              |}
              |""".stripMargin
          )
        )
      },
      testM("scala reserved word used") {
        val schema =
          """
            |type Character {
            |  private: String!
            |  object: String!
            |  type: String!
            |}
            |""".stripMargin

        assertM(gen(schema))(
          stringEqualTo(
            """
              |package caliban.tools.testing
              |
              |object Types {
              |  case class Character(`private`: String, `object`: String, `type`: String)
              |}
              |""".stripMargin
          )
        )
      }
    ) @@ TestAspect.sequential
}
