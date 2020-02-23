package caliban

import zio.test._
import Assertion._
import TestUtils._
import caliban.GraphQL._

object RenderingSpec
    extends DefaultRunnableSpec(
      suite("rendering")(
        test("it should render directives") {
          assert(
            graphQL(resolver).render.trim,
            equalTo("""schema {
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
                      |  role: Role
                      |}
                      |
                      |type Captain {
                      |  shipName: String!
                      |}
                      |
                      |type Character @key({name: "name"}) {
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
                      |}""".stripMargin.trim)
          )
        }
      )
    )
