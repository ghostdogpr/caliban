package caliban.tools

import caliban.parsing.Parser
import zio.RIO
import zio.blocking.Blocking
import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment

object SchemaWriterSpec extends DefaultRunnableSpec {

  def gen(
    schema: String,
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    imports: List[String] = List.empty,
    scalarMappings: Map[String, String] = Map.empty,
    isEffectTypeAbstract: Boolean = false,
    preserveInputNames: Boolean = false
  ): RIO[Blocking, String] = Parser
    .parseQuery(schema.stripMargin)
    .flatMap(doc =>
      Formatter
        .format(
          SchemaWriter.write(
            doc,
            packageName,
            effect,
            Some(imports),
            Some(scalarMappings),
            isEffectTypeAbstract,
            preserveInputNames
          ),
          None
        )
    )

  val assertions = List(
    (
      "type with field parameter",
      gen("""
          type Hero {
                name(pad: Int!): String!
                nick: String!
                bday: Int
              }
            |"""),
      """  object Types {
        |  final case class HeroNameArgs(pad: Int)
        |  final case class Hero(name: HeroNameArgs => String, nick: String, bday: scala.Option[Int])
        |
        |}"""
    ),
    (
      "simple queries",
      gen("""
         type Query {
           user(id: Int): User
           userList: [User]!
         }
         type User {
           id: Int
           name: String
           profilePic: String
         }"""),
      """import Types._
        |
        |object Types {
        |  final case class QueryUserArgs(id: scala.Option[Int])
        |  final case class User(id: scala.Option[Int], name: scala.Option[String], profilePic: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query(
        |    user: QueryUserArgs => zio.UIO[scala.Option[User]],
        |    userList: zio.UIO[List[scala.Option[User]]]
        |  )
        |
        |}"""
    ),
    (
      "simple mutation",
      gen("""
         type Mutation {
           setMessage(message: String): String
         }
         """),
      """import Types._
        |
        |object Types {
        |  final case class MutationSetMessageArgs(message: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Mutation(
        |    setMessage: MutationSetMessageArgs => zio.UIO[scala.Option[String]]
        |  )
        |
        |}"""
    ),
    (
      "simple subscription",
      gen("""
         type Subscription {
           UserWatch(id: Int!): String!
         }
         """),
      """import Types._
        |
        |import zio.stream.ZStream
        |
        |object Types {
        |  final case class SubscriptionUserWatchArgs(id: Int)
        |
        |}
        |
        |object Operations {
        |
        |  final case class Subscription(
        |    UserWatch: SubscriptionUserWatchArgs => ZStream[Any, Nothing, String]
        |  )
        |
        |}"""
    ),
    (
      "simple queries with abstracted effect type",
      gen(
        """
         type Query {
           user(id: Int): User
           userList: [User]!
         }
         type User {
           id: Int
           name: String
           profilePic: String
         }""",
        effect = "F",
        isEffectTypeAbstract = true
      ),
      """import Types._
        |
        |object Types {
        |  final case class QueryUserArgs(id: scala.Option[Int])
        |  final case class User(id: scala.Option[Int], name: scala.Option[String], profilePic: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query[F[_]](
        |    user: QueryUserArgs => F[scala.Option[User]],
        |    userList: F[List[scala.Option[User]]]
        |  )
        |
        |}"""
    ),
    (
      "simple mutation with abstracted effect type",
      gen(
        """
         type Mutation {
           setMessage(message: String): String
         }
         """,
        effect = "F",
        isEffectTypeAbstract = true
      ),
      """import Types._
        |
        |object Types {
        |  final case class MutationSetMessageArgs(message: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Mutation[F[_]](
        |    setMessage: MutationSetMessageArgs => F[scala.Option[String]]
        |  )
        |
        |}"""
    ),
    (
      "schema test",
      gen("""
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
          |"""),
      """import Types._
        |
        |import zio.stream.ZStream
        |
        |object Types {
        |  final case class MutationAddPostArgs(author: scala.Option[String], comment: scala.Option[String])
        |  final case class Post(author: scala.Option[String], comment: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query(
        |    posts: zio.UIO[scala.Option[List[scala.Option[Post]]]]
        |  )
        |
        |  final case class Mutation(
        |    addPost: MutationAddPostArgs => zio.UIO[scala.Option[Post]]
        |  )
        |
        |  final case class Subscription(
        |    postAdded: ZStream[Any, Nothing, scala.Option[Post]]
        |  )
        |
        |}"""
    ),
    ("empty schema test", gen(""), System.lineSeparator),
    (
      "enum type",
      gen("""
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """),
      """object Types {
        |
        |  sealed trait Origin extends scala.Product with scala.Serializable
        |
        |  object Origin {
        |    case object EARTH extends Origin
        |    case object MARS  extends Origin
        |    case object BELT  extends Origin
        |  }
        |
        |}"""
    ),
    (
      "union type",
      gen(s"""
              \"\"\"
             role
             Captain or Pilot
             \"\"\"
             union Role = Captain | Pilot
              \"\"\"
             role2
             Captain or Pilot or Stewart
             \"\"\"
             union Role2 = Captain | Pilot | Stewart

             type Captain {
               "ship" shipName: String!
             }

             type Pilot {
               shipName: String!
             }

             type Stewart {
               shipName: String!
             }
            """),
      s"""import caliban.schema.Annotations._
         |
         |object Types {
         |
         |  @GQLDescription(\"\"\"role
         |Captain or Pilot\"\"\")
         |  sealed trait Role extends scala.Product with scala.Serializable
         |  @GQLDescription(\"\"\"role2
         |Captain or Pilot or Stewart\"\"\")
         |  sealed trait Role2 extends scala.Product with scala.Serializable
         |
         |  object Role2 {
         |    final case class Stewart(shipName: String) extends Role2
         |  }
         |
         |  final case class Captain(
         |    @GQLDescription("ship")
         |    shipName: String
         |  ) extends Role
         |      with Role2
         |  final case class Pilot(shipName: String) extends Role with Role2
         |
         |}"""
    ),
    (
      "GQLDescription with escaped quotes",
      gen(s"""
             type Captain {
               "foo \\"quotes\\" bar" shipName: String!
             }
            """),
      """import caliban.schema.Annotations._
        |
        |object Types {
        |
        |  final case class Captain(
        |    @GQLDescription("foo \"quotes\" bar")
        |    shipName: String
        |  )
        |
        |}"""
    ),
    (
      "schema",
      gen("""
             schema {
               query: Queries
             }

             type Queries {
               characters: Int!
             }
            """),
      """object Operations {
        |
        |  final case class Queries(
        |    characters: zio.UIO[Int]
        |  )
        |
        |}"""
    ),
    (
      "input type",
      gen("""
             type Character {
                name: String!
             }

             input CharacterArgs {
               name: String!
             }
            """),
      """object Types {
        |
        |  final case class Character(name: String)
        |  final case class CharacterArgs(name: String)
        |
        |}"""
    ),
    (
      "input type with preserved input",
      gen(
        """
             type Character {
                name: String!
             }

             input CharacterInput {
               name: String!
             }
            """,
        preserveInputNames = true
      ),
      """import caliban.schema.Annotations._
        |
        |object Types {
        |
        |  final case class Character(name: String)
        |  @GQLInputName("CharacterInput")
        |  final case class CharacterInput(name: String)
        |
        |}"""
    ),
    (
      "scala reserved word used",
      gen("""
             type Character {
               private: String!
               object: String!
               type: String!
             }
            """),
      """ object Types {
        |
        |  final case class Character(`private`: String, `object`: String, `type`: String)
        |
        |}"""
    ),
    (
      "final case class reserved field name used",
      gen("""
             type Character {
               wait: String!
             }
            """),
      """object Types {
        |
        |  final case class Character(wait$ : String)
        |
        |}"""
    ),
    (
      "args unique class names",
      gen("""
          |type Hero {
          |  callAllies(number: Int!): [Hero!]!
          |}
          |
          |type Villain {
          |  callAllies(number: Int!, w: String!): [Villain!]!
          |}
            """),
      """object Types {
        |  final case class HeroCallAlliesArgs(number: Int)
        |  final case class VillainCallAlliesArgs(number: Int, w: String)
        |  final case class Hero(callAllies: HeroCallAlliesArgs => List[Hero])
        |  final case class Villain(callAllies: VillainCallAlliesArgs => List[Villain])
        |
        |}"""
    ),
    (
      "args names root level",
      gen("""
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
            """),
      """import Types._
        |
        |import zio.stream.ZStream
        |
        |object Types {
        |  final case class QueryCharactersArgs(p: Params)
        |  final case class SubscriptionCharactersArgs(p: Params)
        |  final case class Params(p: Int)
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query(
        |    characters: QueryCharactersArgs => zio.UIO[Int]
        |  )
        |
        |  final case class Subscription(
        |    characters: SubscriptionCharactersArgs => ZStream[Any, Nothing, Int]
        |  )
        |
        |}"""
    ),
    (
      "add scalar mappings and additional imports",
      gen(
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
          |""",
        scalarMappings = Map("OffsetDateTime" -> "java.time.OffsetDateTime"),
        imports = List("java.util.UUID", "a.b._")
      ),
      """import Types._
        |
        |import zio.stream.ZStream
        |
        |import java.util.UUID
        |import a.b._
        |
        |object Types {
        |  final case class MutationAddPostArgs(author: scala.Option[String], comment: scala.Option[String])
        |  final case class Post(date: java.time.OffsetDateTime, author: scala.Option[String], comment: scala.Option[String])
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query(
        |    posts: zio.UIO[scala.Option[List[scala.Option[Post]]]]
        |  )
        |
        |  final case class Mutation(
        |    addPost: MutationAddPostArgs => zio.UIO[scala.Option[Post]]
        |  )
        |
        |  final case class Subscription(
        |    postAdded: ZStream[Any, Nothing, scala.Option[Post]]
        |  )
        |
        |}"""
    ),
    (
      "interface type",
      gen(
        s"""
              \"\"\"
              person
              Admin or Customer
             \"\"\"
            interface Person {
                id: ID!
                firstName: String!
                lastName: String!
             }

             type Admin implements Person {
               id: ID!
               "firstName" firstName: String!
               lastName: String!
             }

             type Customer implements Person {
               id: ID!
               firstName: String!
               lastName: String!
               email: String!
             }
            """,
        scalarMappings = Map("ID" -> "java.util.UUID")
      ),
      s"""import caliban.schema.Annotations._
         |
         |object Types {
         |
         |  @GQLInterface
         |  @GQLDescription(\"\"\"person
         |Admin or Customer\"\"\")
         |  sealed trait Person extends scala.Product with scala.Serializable {
         |    def id: java.util.UUID
         |    def firstName: String
         |    def lastName: String
         |  }
         |
         |  object Person {
         |    final case class Admin(
         |      id: java.util.UUID,
         |      @GQLDescription("firstName")
         |      firstName: String,
         |      lastName: String
         |    ) extends Person
         |    final case class Customer(id: java.util.UUID, firstName: String, lastName: String, email: String) extends Person
         |  }
         |
         |}"""
    )
  )

  override def spec: ZSpec[TestEnvironment, Any] = suite("SchemaWriterSpec")(
    assertions.map { case (name, actual, expected) =>
      testM(name)(
        assertM(actual.map(_.stripMargin.trim))(equalTo(expected.stripMargin.trim))
      )
    }: _*
  ) @@ TestAspect.sequential
}
