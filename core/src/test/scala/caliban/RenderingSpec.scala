package caliban

import caliban.GraphQL._
import caliban.TestUtils._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.Directive
import zio.test.Assertion._
import zio.test._

import scala.annotation.tailrec

object RenderingSpec extends ZIOSpecDefault {

  val tripleQuote = "\"\"\""

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
          equalTo("\"\"\"\nA multiline \"TestType\" description\ngiven inside \\\"\"\"-quotes\n\n\"\"\"\ntype TestType")
        )
      },
      test("it should render single line descriptions") {
        import RenderingSpecSchemaSingleLineDescription.resolver
        val expected =
          """schema {
            |  query: Query
            |}
            |
            |"type description in a single line"
            |type OutputValue {
            |  "field description in a single line"
            |  r: Int!
            |}
            |
            |type Query {
            |  "query description in a single line"
            |  q("argument description in a single line" in: Int!): OutputValue!
            |}
            |""".stripMargin
        assert(graphQL(resolver).render.trim)(equalTo(expected.trim))
      },
      test("it should render multiple line descriptions") {
        import RenderingSpecSchemaMultiLineDescription.resolver
        val expected =
          s"""schema {
             |  query: Query
             |}
             |
             |$tripleQuote
             |type description in
             |Multiple lines
             |$tripleQuote
             |type OutputValue {
             |  $tripleQuote
             |field description in
             |Multiple lines
             |$tripleQuote
             |  r: Int!
             |}
             |
             |type Query {
             |  $tripleQuote
             |query description in
             |Multiple lines
             |$tripleQuote
             |  q(${tripleQuote}argument description in
             |Multiple lines${tripleQuote} in: Int!): OutputValue!
             |}
             |""".stripMargin
        assert(graphQL(resolver).render.trim)(equalTo(expected.trim))
      },
      test("it should render single line descriptions ending in quote") {
        import RenderingSpecSchemaSingleLineEndingInQuoteDescription.resolver
        val expected =
          """schema {
            |  query: Query
            |}
            |
            |"type description in a single line \"ending in quote\""
            |type OutputValue {
            |  "field description in a single line \"ending in quote\""
            |  r: Int!
            |}
            |
            |type Query {
            |  "query description in a single line \"ending in quote\""
            |  q("argument description in a single line \"ending in quote\"" in: Int!): OutputValue!
            |}
            |""".stripMargin
        assert(graphQL(resolver).render.trim)(equalTo(expected.trim))
      },
      test("it should render multi line descriptions ending in quote") {
        import RenderingSpecSchemaMultiLineEndingInQuoteDescription.resolver
        val expected =
          s"""schema {
             |  query: Query
             |}
             |
             |$tripleQuote
             |type description in multiple lines
             |\"ending in quote\"
             |$tripleQuote
             |type OutputValue {
             |  $tripleQuote
             |field description in multiple lines
             |\"ending in quote\"
             |$tripleQuote
             |  r: Int!
             |}
             |
             |type Query {
             |  $tripleQuote
             |query description in multiple lines
             |\"ending in quote\"
             |$tripleQuote
             |  q(${tripleQuote}argument description in multiple lines
             |\"ending in quote\" ${tripleQuote} in: Int!): OutputValue!
             |}
             |""".stripMargin
        assert(graphQL(resolver).render.trim)(equalTo(expected.trim))
      }
    )

  sealed trait DiffResult {
    def areEqual: Boolean
    def commonPrefix: String
    def diffPrefix: (String, String)
  }

  case class EqualResult(s: String) extends DiffResult {
    override def areEqual: Boolean = true

    override def commonPrefix: String = s

    override def diffPrefix: (String, String) = ("", "")
  }

  case class DifferentResult(prefix: String, left1: String, left2: String) extends DiffResult {
    override def areEqual: Boolean = false

    override def commonPrefix: String = prefix

    override def diffPrefix: (String, String) = (left1, left2)
  }

  def displayFirstDifference(s1: String, s2: String) = {
    @tailrec
    def loop(currS1: List[Char], currS2: List[Char], soFar: List[Char]): DiffResult =
      (currS1, currS2) match {
        case (Nil, Nil)                                 => EqualResult(s1)
        case (Nil, _)                                   => DifferentResult(soFar.reverse.mkString, currS1.mkString, currS2.mkString)
        case (_, Nil)                                   => DifferentResult(soFar.reverse.mkString, currS1.mkString, currS2.mkString)
        case ((s1h :: s1t), (s2h :: s2t)) if s1h == s2h =>
          loop(s1t, s2t, s1h :: soFar)
        case (_, _)                                     => DifferentResult(soFar.reverse.mkString, currS1.mkString, currS2.mkString)
      }
    loop(s1.toList, s2.toList, List.empty)

  }
}
