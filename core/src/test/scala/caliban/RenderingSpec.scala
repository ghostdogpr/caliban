package caliban

import caliban.GraphQL._
import caliban.TestUtils._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object RenderingSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("rendering")(
      test("it should render directives") {
        assert(graphQL(resolver).render.trim)(
          equalTo("""schema {
                    |  query: Query
                    |}
                    |
                    |"Description of custom scalar emphasizing proper captain ship names"
                    |scalar CaptainShipName
                    |
                    |union Role = Captain | Engineer | Mechanic | Pilot
                    |
                    |enum Origin {
                    |  BELT
                    |  EARTH
                    |  MARS
                    |  MOON @deprecated(reason: "Use: EARTH | MARS | BELT")
                    |}
                    |
                    |input CharacterInput {
                    |  name: String! @external
                    |  nicknames: [String!]! @required
                    |  origin: Origin!
                    |}
                    |
                    |type Captain {
                    |  shipName: CaptainShipName!
                    |}
                    |
                    |type Character @key(name: "name") {
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
                    |}""".stripMargin.trim)
        )
      },
      test("it should render descriptions") {
        import RenderingSpecSchema._
        val generated = graphQL(resolverSchema).render.trim
        assert(generated)(equalTo("""|schema {
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
                                     |}""".stripMargin.trim))
      }
    )
}
