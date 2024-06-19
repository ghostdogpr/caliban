package caliban.tools

import caliban.parsing.Parser
import zio.{ Task, ZIO }
import zio.test._

object ClientWriterSpec extends SnapshotTest {
  override val testName: String = "ClientWriterSpec"
  def gen(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty,
    additionalImports: List[String] = List.empty,
    extensibleEnums: Boolean = false,
    excludeDeprecated: Boolean = false,
    genView: Boolean = false
  ): Task[String] = ZIO
    .fromEither(Parser.parseQuery(schema))
    .flatMap(doc =>
      Formatter.format(
        ClientWriter
          .write(
            doc,
            additionalImports = Some(additionalImports),
            extensibleEnums = extensibleEnums,
            scalarMappings = Some(scalarMappings),
            excludeDeprecated = excludeDeprecated,
            genView = genView
          )
          .head
          ._2,
        None
      )
    )

  def genSplit(
    schema: String,
    scalarMappings: Map[String, String] = Map.empty
  ): Task[List[(String, String)]] = ZIO
    .fromEither(Parser.parseQuery(schema))
    .flatMap(doc =>
      Formatter.format(
        ClientWriter.write(doc, packageName = Some("test"), splitFiles = true, scalarMappings = Some(scalarMappings)),
        None
      )
    )

  override def spec =
    suite("ClientWriterSpec")(
      snapshotTest("simple object type") {
        val schema =
          """
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """

        gen(schema)
      },
      snapshotTest("simple object type with exclude deprecated and genView") {
        gen(
          schema = """
             type Character {
               name: String!
               nicknames: [String!]! @deprecated
             }
            """,
          excludeDeprecated = true,
          genView = true
        )
      },
      snapshotTest("simple object type with exclude deprecated and genView, only deprecated fields") {
        gen(
          schema = """
             type Character {
               name: String! @deprecated
               nicknames: [String!]! @deprecated(reason: "blah")
             }
            """,
          excludeDeprecated = true,
          genView = true
        )
      },
      snapshotTest("object type with reserved name") {
        gen("""
             type Character {
               type: String!
             }
            """)
      },
      snapshotTest("nested object type") {
        gen("""
             type Q {
               characters: [Character!]!
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """)
      },
      snapshotTest("object type with arguments") {
        gen("""
             type Q {
               character(name: String!): Character
             }

             type Character {
               name: String!
               nicknames: [String!]!
             }
            """)
      },
      snapshotTest("schema") {
        gen("""
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
            """)
      },
      snapshotTest("enum") {
        gen("""
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """)
      },
      snapshotTest("enum with exclude deprecated") {
        gen(
          schema = """
             enum Origin {
               EARTH
               MARS @deprecated
               BELT
             }
            """,
          excludeDeprecated = true
        )
      },
      snapshotTest("enum with exclude deprecated, only deprecated values") {
        gen(
          schema = """
             enum Origin {
               MARS @deprecated
               BELT @deprecated(reason: "blah")
             }
            """,
          excludeDeprecated = true
        )
      },
      snapshotTest("scalar mapped enum") {
        gen(
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }

             enum Destination {
               EARTH
               MARS
               BELT
             }

             input Routes {
               origin: Origin!
               destinations: [Destination!]!
             }
            """,
          scalarMappings = Map("Destination" -> "com.example.Destination")
        )
      },
      snapshotTest("extensible enum") {
        gen(
          """
             enum Origin {
               EARTH
               MARS
               BELT
             }
            """,
          extensibleEnums = true
        )
      },
      snapshotTest("input object") {
        gen("""
             input CharacterInput {
               name: String!
               nicknames: [String!]!
             }
            """)
      },
      snapshotTest("input object oneOf") {
        gen("""
             input CharacterInput @oneOf {
               name: String
               nicknames: [String!]
             }
            """)
      },
      snapshotTest("input object with reserved name") {
        gen("""
             input CharacterInput {
               wait: String!
             }
            """)
      },
      snapshotTest("union") {
        gen("""
             union Role = Captain_ | Pilot

             type Captain_ {
               shipName: String!
             }

             type Pilot {
               shipName: String!
             }

             type Character {
               role: Role
             }
            """)
      },
      snapshotTest("deprecated field + comment") {
        gen("""
             type Character {
               "name"
               name: String! @deprecated(reason: "blah")
               nicknames: [String!]! @deprecated
             }
            """)
      },
      snapshotTest("deprecated field + comment newline") {
        gen("""
             type Character {
               "name"
               name: String! @deprecated(reason: "foo\nbar")
             }
            """)
      },
      snapshotTest("default arguments for optional and list arguments") {
        gen("""
              type Query {
                characters(
                  first: Int!
                  last: Int
                  origins: [String]!
                ): String
              }""")
      },
      snapshotTest("support for Json scalar") {
        gen(
          """
              scalar Json

              type Query {
                test: Json!
              }""",
          Map("Json" -> "io.circe.Json")
        )
      },
      snapshotTest("case-sensitive name uniqueness in enum's values") {
        gen("""
              enum Episode {
                NEWHOPE
                EMPIRE
                JEDI
                jedi
              }
            """)
      },
      snapshotTest("case-insensitive name uniqueness in 2 basic objects") {
        gen("""
             type Character {
               name: String!
               nicknames: [String!]!
             }

             type character {
               name: String!
               nicknames: [String!]!
             }
            """)
      },
      snapshotTest("safe names with underscores") {
        gen("""
             type Character {
               _: Boolean # Fake field because GraphQL does not support empty objects
               _name_: String
               _nickname: String
               age_: Int
             }
            """)
      },
      snapshotTest("add scalar mappings and additional imports") {
        gen(
          """
             scalar OffsetDateTime

             type Order {
               date: OffsetDateTime!
             }
            """,
          Map("OffsetDateTime" -> "java.time.OffsetDateTime"),
          List("java.util.UUID", "a.b._")
        )
      },
      snapshotTest("schema with splitFiles") {
        genSplit("""
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
            """).map(_.map { case (name, str) => s"--- $name ---\n$str" }.mkString("\n"))
      },
      snapshotTest("interface") {
        gen(
          """
             interface Order {
               name: String!
             }
             type Ascending implements Order {
               name: String!
             }
             type Sort {
               object: Order
             }
            """,
          Map.empty,
          List.empty
        )
      },
      snapshotTest("interface with list") {
        gen(
          """
             interface Order {
               name: String!
             }
             type Ascending implements Order {
               name: String!
             }
             type Descending implements Order {
               name: String!
             }
             type Sort {
               orders: [Order]
             }
            """,
          Map.empty,
          List.empty
        )
      },
      snapshotTest("interface without implements") {
        gen("""
             interface Entity {
               statusInfo: StatusInfo!
             }
             enum Status {
               Status1
             }
             interface StatusInfo {
               status: Status!
             }
            """)
      },
      snapshotTest("interface with implements") {
        gen("""
             interface Entity {
               name: String!
             }
             interface Pet implements Entity {
               name: String!
             }
             type Cat implements Pet {
               name: String!
             }
             type Dog implements Pet {
               name: String!
             }
            """)
      },
      snapshotTest("Option types don't conflict with scala.Option") {
        gen("""
             type Option {
               id: UUID!
               name: String
               character: Character
             }
             type Character {
               name: String!
               nicknames: [String!]!
             }
             type Query {
               maybeOption(id: UUID!): Option
               someOption(id: UUID!): Option!
               maybeOptionList(id: UUID!): [Option!]
               someOptionList(id: UUID!): [Option!]!
               maybeMaybeOptionList: [Option]
             }
            """)
      }
    ) @@ TestAspect.parallelN(4)
}
