package caliban

import caliban.GraphQL._
import caliban.TestUtils._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.Directive
import zio.test.Assertion._
import zio.test._

object RenderingSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
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
      }
    )
}
