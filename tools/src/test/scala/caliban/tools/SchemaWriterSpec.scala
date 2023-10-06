package caliban.tools

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion.equalTo
import zio.test._

object SchemaWriterSpec extends ZIOSpecDefault {

  def gen(
    schema: String,
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    imports: List[String] = List.empty,
    scalarMappings: Map[String, String] = Map.empty,
    isEffectTypeAbstract: Boolean = false,
    preserveInputNames: Boolean = false,
    addDerives: Boolean = false
  ): Task[String] = Parser
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
            preserveInputNames,
            addDerives
          ),
          Some(".scalafmt-for-test.conf")
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
    ),
    (
      "add derives",
      gen(
        """
        |type Hero {
        |  name(pad: Int!): String!
        |}
        |
        |enum Episode {
        |  NEWHOPE
        |  EMPIRE
        |  JEDI
        |}
        |
        |type Query {
        |  hero(episode: Episode): Hero
        |}
        |
        |input HeroInput {
        |  name: String!
        |}
        |
        |""",
        addDerives = true
      ),
      """import Types._
        |
        |object Types {
        |  final case class HeroNameArgs(pad: Int) derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
        |  final case class QueryHeroArgs(episode: scala.Option[Episode])
        |      derives caliban.schema.Schema.SemiAuto,
        |        caliban.schema.ArgBuilder
        |  final case class Hero(name: HeroNameArgs => String) derives caliban.schema.Schema.SemiAuto
        |  final case class HeroInput(name: String) derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
        |
        |  sealed trait Episode extends scala.Product with scala.Serializable
        |      derives caliban.schema.Schema.SemiAuto,
        |        caliban.schema.ArgBuilder
        |
        |  object Episode {
        |    case object NEWHOPE extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
        |    case object EMPIRE  extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
        |    case object JEDI    extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
        |  }
        |
        |}
        |
        |object Operations {
        |
        |  final case class Query(
        |    hero: QueryHeroArgs => zio.UIO[scala.Option[Hero]]
        |  ) derives caliban.schema.Schema.SemiAuto
        |
        |}""".stripMargin
    ),
    (
      "inherits field with args",
      gen(
        """
        |interface Character {
        |    friendsConnection(first: Int, after: ID): FriendsConnection!
        |}
        |type Human implements Character {
        |    friendsConnection(first: Int, after: ID): FriendsConnection!
        |}
        |type Droid implements Character {
        |    friendsConnection(first: Int, after: ID): FriendsConnection!
        |}""",
        addDerives = true
      ),
      """import caliban.schema.Annotations._
        |
        |object Types {
        |  final case class CharacterFriendsConnectionArgs(first: scala.Option[Int], after: scala.Option[ID])
        |      derives caliban.schema.Schema.SemiAuto,
        |        caliban.schema.ArgBuilder
        |
        |  @GQLInterface
        |  sealed trait Character extends scala.Product with scala.Serializable {
        |    def friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection
        |  }
        |
        |  object Character {
        |    final case class Human(friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection) extends Character
        |        derives caliban.schema.Schema.SemiAuto
        |    final case class Droid(friendsConnection: CharacterFriendsConnectionArgs => FriendsConnection) extends Character
        |        derives caliban.schema.Schema.SemiAuto
        |  }
        |
        |}""".stripMargin
    )
  )

  override def spec = suite("SchemaWriterSpec")(
    assertions.map { case (name, actual, expected) =>
      test(name)(
        actual.map(_.stripMargin.trim).map { str =>
          assertTrue(str == expected.stripMargin.trim)
        }
      )
    }: _*
  ) @@ TestAspect.sequential
}
