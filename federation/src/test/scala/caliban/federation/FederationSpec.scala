package caliban.federation

import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.TestUtils._
import zio.test._
import Assertion._
import zquery.ZQuery

object FederationSpec extends DefaultRunnableSpec {
  override def spec = suite("FederationSpec")(
    testM("resolve federated types") {
      val interpreter = federate(
        graphQL(resolver),
        EntityResolver[Any, CharacterArgs, Character](args => ZQuery.succeed(characters.find(_.name == args.name))) :: Nil
      ).interpreter

      val query = gqldoc("""
            query test {
              _entities(representations: [{__typename: "Character", name: "Amos Burton"}]) {
                  __typename
                  ... on Character {
                    name
                  }
              }
            }""")

      assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
        equalTo("""{"_entities":[{"__typename":"Character","name":"Amos Burton"}]}""")
      )
    }
  )
}
