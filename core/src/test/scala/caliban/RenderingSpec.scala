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
        assert(graphQL(resolver).render.trim)(equalTo("""schema {
                                                        |  query: Query
                                                        |}
                                                        |
                                                        |union Role = Captain | Engineer | Mechanic | Pilot
                                                        |
                                                        |enum Origin {
                                                        |  BELT
                                                        |  EARTH
                                                        |  MARS
                                                        |}
                                                        |
                                                        |input CharacterInput {
                                                        |  name: String! @external
                                                        |  nicknames: [String!]! @required
                                                        |  origin: Origin!
                                                        |}
                                                        |
                                                        |type Captain {
                                                        |  shipName: String!
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
                                                        |  characters(origin: Origin): [Character!]!
                                                        |  charactersIn(names: [String!]!): [Character!]!
                                                        |  exists(character: CharacterInput!): Boolean!
                                                        |}""".stripMargin.trim))
      }
    )
}
