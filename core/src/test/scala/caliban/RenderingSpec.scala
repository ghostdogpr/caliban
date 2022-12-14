package caliban

import caliban.GraphQL._
import caliban.TestUtils._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.Directive
import zio.test.Assertion._
import zio.test._

object RenderingSpec extends ZIOSpecDefault {

  override def spec =
    suite("rendering")(
      test("it should render directives") {
        assert(
          graphQL(
            resolver,
            directives = List(Directives.Test),
            schemaDirectives = List(SchemaDirectives.Link)
          ).render.trim
        )(
          equalTo(
            """"Test directive"
              |directive @test(foo: Int) on FIELD_DEFINITION
              |
              |schema @link(url: "https://example.com", import: ["@key", {name: "@provides", as: "@self"}]) {
              |  query: Query
              |}
              |
              |"Description of custom scalar emphasizing proper captain ship names"
              |scalar CaptainShipName @specifiedBy(url: "http://someUrl")
              |
              |union Role @uniondirective = Captain | Engineer | Mechanic | Pilot
              |
              |enum Origin @enumdirective {
              |  BELT
              |  EARTH
              |  MARS
              |  MOON @deprecated(reason: "Use: EARTH | MARS | BELT")
              |}
              |
              |input CharacterInput @inputobjdirective {
              |  name: String! @external
              |  nicknames: [String!]! @required
              |  origin: Origin!
              |}
              |
              |interface Human {
              |  name: String! @external
              |}
              |
              |type Captain {
              |  shipName: CaptainShipName!
              |}
              |
              |type Character implements Human @key(name: "name") {
              |  name: String! @external
              |  nicknames: [String!]! @required
              |  origin: Origin!
              |  role: Role
              |}
              |
              |type Engineer {
              |  shipName: String!
              |}
              |
              |type Mechanic {
              |  shipName: String!
              |}
              |
              |type Narrator implements Human {
              |  name: String!
              |}
              |
              |type Pilot {
              |  shipName: String!
              |}
              |
              |"Queries"
              |type Query {
              |  "Return all characters from a given origin"
              |  characters(origin: Origin): [Character!]!
              |  character(name: String!): Character @deprecated(reason: "Use `characters`")
              |  charactersIn(names: [String!]!): [Character!]!
              |  exists(character: CharacterInput!): Boolean!
              |  human: Human!
              |}""".stripMargin.trim
          )
        )
      },
      test("it should render descriptions") {
        import RenderingSpecSchema._
        val generated = graphQL(resolverSchema).render.trim
        assertTrue(generated == """|schema {
                                   |  query: QueryTest
                                   |  mutation: MutationTest
                                   |}
                                   |
                                   |input UserTestInput {
                                   |  name: String!
                                   |  "field-description"
                                   |  age: Int!
                                   |}
                                   |
                                   |type MutationTest {
                                   |  id(id: Int!, user: UserTestInput!): Boolean!
                                   |  fetch(nameLike: String!, "is user active currently" active: Boolean!): Boolean!
                                   |}
                                   |
                                   |type QueryTest {
                                   |  allUsers: [UserTest!]!
                                   |}
                                   |
                                   |type UserTest {
                                   |  name: String!
                                   |  "field-description"
                                   |  age: Int!
                                   |}""".stripMargin.trim)
      },
      test("it should render empty objects without field list") {
        assertTrue(graphQL(InvalidSchemas.Object.resolverEmpty).render.trim == """schema {
                                                                                 |  query: TestEmptyObject
                                                                                 |}
                                                                                 |
                                                                                 |type EmptyObject
                                                                                 |
                                                                                 |type TestEmptyObject {
                                                                                 |  o: EmptyObject!
                                                                                 |}""".stripMargin.trim)
      },
      test("it should not render a schema in no queries, mutations, or subscription") {
        assert(graphQL(InvalidSchemas.resolverEmpty).render.trim)(
          equalTo("")
        )
      },
      test("it should render object arguments in type directives") {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          directives = Some(
            List(
              Directive(
                name = "testdirective",
                arguments = Map(
                  "object" -> InputValue.ObjectValue(
                    Map(
                      "key1" -> Value.StringValue("value1"),
                      "key2" -> Value.StringValue("value2")
                    )
                  )
                )
              )
            )
          )
        )
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(equalTo("type TestType @testdirective(object: {key1: \"value1\",key2: \"value2\"})"))
      },
      test(
        "it should escape \", \\, backspace, linefeed, carriage-return and tab inside a normally quoted description string"
      ) {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A \"TestType\" description with \\, \b, \f, \r and \t")
        )
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(
          equalTo("\"A \\\"TestType\\\" description with \\\\, \\b, \\f, \\r and \\t\"\ntype TestType")
        )
      },
      test("it should escape \"\"\" inside a triple-quoted description string") {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A multiline \"TestType\" description\ngiven inside \"\"\"-quotes\n")
        )
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(
          equalTo("\"\"\"\nA multiline \"TestType\" description\ngiven inside \\\"\"\"-quotes\n\"\"\"\ntype TestType")
        )
      },
      test("it should handle descriptions ending in '\"' properly") {
        import RenderingSpecSchemaDescriptions._
        val tripleQuote = "\"\"\""
        val expected    =
          s"""schema {
             |  query: Query
             |}
             |
             |type Query {
             |  "query. Single line"
             |  getUser1("argument single line" id: Int!): TheResult!
             |  ${tripleQuote}
             |query.
             |Multi line${tripleQuote}
             |  getUser2(${tripleQuote}argument
             |Multi line${tripleQuote} id: Int!): TheResult!
             |  "query. Single line ending in \\\"quote\\\""
             |  getUser3("argument single line ending in \\\"quote\\\"" id: Int!): TheResult!
             |  ${tripleQuote}
             |query.
             |Multi line ending in "quote"
             |${tripleQuote}
             |  getUser4(${tripleQuote}argument
             |Multi line ending in "quote" ${tripleQuote} id: Int!): TheResult!
             |}
             |
             |type R1 {
             |  name: String!
             |  "field. Single line"
             |  age: Int!
             |}
             |
             |type R2 {
             |  name: String!
             |  ${tripleQuote}
             |field.
             |Multi line${tripleQuote}
             |  age: Int!
             |}
             |
             |type R3 {
             |  name: String!
             |  "field. Single line ending in \\\"quote\\\""
             |  age: Int!
             |}
             |
             |type R4 {
             |  name: String!
             |  ${tripleQuote}
             |field.
             |Multi line ending in "quote"
             |${tripleQuote}
             |  age: Int!
             |}
             |
             |type TheResult {
             |  u1: R1!
             |  u2: R2!
             |  u3: R3!
             |  u4: R4!
             |}
             |""".stripMargin
        assert {
          graphQL(resolverForDescriptionTest).render.trim
        }(equalTo(expected.trim))
      }
    )
}
