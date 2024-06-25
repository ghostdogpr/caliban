package caliban.tools

import caliban.parsing.Parser
import caliban.parsing.adt.Directives.NewtypeDirective
import zio.{ Task, ZIO }
import zio.test._

object SchemaWriterSpec extends SnapshotTest {
  override val testName: String = "SchemaWriterSpec"

  def gen(
    schema: String,
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    imports: List[String] = List.empty,
    scalarMappings: Map[String, String] = Map.empty,
    isEffectTypeAbstract: Boolean = false,
    preserveInputNames: Boolean = false,
    addDerives: Boolean = false
  ): Task[String] = ZIO
    .fromEither(Parser.parseQuery(schema.stripMargin))
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

  val assertions = List[Spec[Any, Throwable]](
    snapshotTest("type with field parameter")(gen("""
          type Hero {
                name(pad: Int!): String!
                nick: String!
                bday: Int
              }
            """)),
    snapshotTest("simple queries")(gen("""
         type Query {
           user(id: Int): User
           userList: [User]!
         }
         type User {
           id: Int
           name: String
           profilePic: String
         }""")),
    snapshotTest("simple mutation")(gen("""
         type Mutation {
           setMessage(message: String): String
         }
         """)),
    snapshotTest("simple subscription")(gen("""
         type Subscription {
           UserWatch(id: Int!): String!
         }
         """)),
    snapshotTest("simple queries with abstracted effect type")(
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
      )
    ),
    snapshotTest("simple mutation with abstracted effect type")(
      gen(
        """
         type Mutation {
           setMessage(message: String): String
         }
         """,
        effect = "F",
        isEffectTypeAbstract = true
      )
    ),
    snapshotTest("schema test")(gen("""
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
          |""")),
    snapshotTest("(empty schema test)")(gen("")),
    snapshotTest("enum type")(gen("""
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """)),
    snapshotTest("union type")(
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
            """)
    ),
    snapshotTest("GQLDescription with escaped quotes")(
      gen(s"""
             type Captain {
               "foo \\"quotes\\" bar" shipName: String!
             }
            """)
    ),
    snapshotTest("GQLDeprecated with reason")(
      gen(s"""
             type Captain {
               "foo" shipName: String! @deprecated(reason: "bar")
             }
            """)
    ),
    snapshotTest("GQLDeprecated with multiline reason and escaped quotes")(
      gen(s"""
              type ExampleType {
                oldField: String @deprecated(reason: \"\"\"
                  This field is deprecated for the following reasons:
                  1. "Outdated data model": The field relies on an outdated data model.
                  2. "Performance issues": Queries using this field have performance issues.
                  Please use `newField` instead.
                \"\"\")
                newField: String
              }

            """)
    ),
    snapshotTest("schema")(gen("""
             schema {
               query: Queries
             }

             type Queries {
               characters: Int!
             }
            """)),
    snapshotTest("input type")(gen("""
             type Character {
                name: String!
             }

             input CharacterArgs {
               name: String!
             }
            """)),
    snapshotTest("input type oneOf")(gen("""
             type Character {
                name: String!
             }

             input CharacterArgs @oneOf {
               foo: String
               bar: Int
             }
            """)),
    snapshotTest("input type with preserved input")(
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
      )
    ),
    snapshotTest("scala reserved word used")(gen("""
             type Character {
               private: String!
               object: String!
               type: String!
             }
            """)),
    snapshotTest("final case class reserved field name used")(gen("""
             type Character {
               wait: String!
             }
            """)),
    snapshotTest("args unique class names")(gen("""
          |type Hero {
          |  callAllies(number: Int!): [Hero!]!
          |}
          |
          |type Villain {
          |  callAllies(number: Int!, w: String!): [Villain!]!
          |}
            """)),
    snapshotTest("args names root level")(gen("""
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
            """)),
    snapshotTest("add scalar mappings and additional imports")(
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
      )
    ),
    snapshotTest("interface type")(
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
      )
    ),
    snapshotTest("add derives")(
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
      )
    ),
    snapshotTest("inherits field with args")(
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
      )
    ),
    snapshotTest("recognize @lazy intention and generate side-effecting field")(
      gen(
        """
        |directive @lazy on FIELD_DEFINITION
        |
        |type Foo {
        |  bar: String!
        |  baz: String! @lazy
        |}"""
      )
    ),
    snapshotTest("recognize @lazy intention and generate side-effecting field with abstracted effect type")(
      gen(
        """
        |directive @lazy on FIELD_DEFINITION
        |
        |type Foo {
        |  bar: String!
        |  baz: String! @lazy
        |}""",
        effect = "F",
        isEffectTypeAbstract = true
      )
    ),
    snapshotTest("generate nested @lazy fields with abstracted effect type")(
      gen(
        """
        |directive @lazy on FIELD_DEFINITION
        |
        |type Foo {
        |  bar: Bar!
        |}
        |
        |type Bar {
        |  baz: Baz! @lazy
        |}
        |
        |type Baz {
        |  x: String!
        |  y: String! @lazy
        |}
        |
        |type Query {
        |  foo: Foo!
        |}""",
        effect = "F",
        isEffectTypeAbstract = true
      )
    ),
    snapshotTest("type appears in type union and implements interface")(
      gen(
        """
        |schema {
        |  query: Query
        |}
        |
        |union AllErrors = Bar | Foo | FooBar
        |
        |interface Error {
        |  "description"
        |  message: String!
        |}
        |
        |type Bar implements Error {
        |  message: String!
        |}
        |
        |type Foo implements Error {
        |  message: String!
        |}
        |
        |type FooBar implements Error {
        |  message: String!
        |}
        |
        |type Query {
        |  errorInterface: Error!
        |  errorUnion: AllErrors!
        |}""",
        addDerives = true
      )
    ),
    snapshotTest("generate typesafe ids with @newtype directive, for fields on types, input types and arguments")(
      gen(
        s"""
          |directive @$NewtypeDirective(name : String) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION
          |
          |scalar FID
          |
          |type Query {
          | getFoo(
          |   id: ID! @$NewtypeDirective(name: "CustomId"),
          |   maybeId : ID @$NewtypeDirective(name: "ACustomIdOpt")
          |   mapbeAllIDsOpt: [ID] @$NewtypeDirective(name: "AMaybeInnerIdOpt")
          | ): Foo
          |}
          |
          |type Mutation {
          | updateFoo(foo: FooInput!): Foo
          |}
          |
          |type Foo {
          |  id : ID! @$NewtypeDirective(name: "CustomId")
          |  strId : String! @$NewtypeDirective(name: "CustomStrId")
          |  intId : Int! @$NewtypeDirective(name: "CustomIntId")
          |  fid : FID! @$NewtypeDirective(name: "CustomFId")
          |  maybeId : ID @$NewtypeDirective(name: "CustomIdOpt")
          |  IDs : [ID!]!
          |  allIDs : [ID!]! @$NewtypeDirective(name: "InnerId")
          |  allIDsOpt: [ID]! @$NewtypeDirective(name: "InnerOptId")
          |  mapbeAllIDs: [ID!] @$NewtypeDirective(name: "MaybeInnerId")
          |  mapbeAllIDsOpt: [ID] @$NewtypeDirective(name: "MaybeInnerIdOpt")
          |}
          |
          |input FooInput {
          |  id : ID! @$NewtypeDirective(name: "CustomId")
          |  allIDs : [ID!]! @$NewtypeDirective(name: "IInnerId")
          |  mapbeAllIDsOpt: [ID] @$NewtypeDirective(name: "IMaybeInnerIdOpt")
          |}
          |
          |""",
        scalarMappings = Map("ID" -> "String")
      )
    )
  )

  override def spec =
    suite("SchemaWriterSpec")(assertions) @@ TestAspect.parallelN(4)
}
