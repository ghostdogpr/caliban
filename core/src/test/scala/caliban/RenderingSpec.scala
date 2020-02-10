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
            equalTo("""
                      |union Role = Captain | Engineer | Mechanic | Pilot
                      |
                      |type Engineer  {
                      |  shipName: String! 
                      |}
                      |
                      |input CharacterInput  {
                      |  name: String!
                      |  nicknames: [String!]!
                      |  origin: Origin!
                      |  role: Role
                      |}
                      |
                      |enum Origin  {
                      |  BELT
                      |  EARTH
                      |  MARS
                      |}
                      |
                      |"Queries"
                      |type Query  {
                      |  characters(origin: Origin): [Character!]! 
                      |  charactersIn(names: [String!]!): [Character!]! 
                      |  exists(character: CharacterInput!): Boolean! 
                      |}
                      |
                      |type Character @key({name: "name"}) {
                      |  name: String! @external
                      |  nicknames: [String!]! @required
                      |  origin: Origin! 
                      |  role: Role 
                      |}
                      |
                      |type Pilot  {
                      |  shipName: String! 
                      |}
                      |
                      |type Mechanic  {
                      |  shipName: String! 
                      |}
                      |
                      |type Captain  {
                      |  shipName: String! 
                      |}""".stripMargin.trim)
          )
        }
      )
    )
