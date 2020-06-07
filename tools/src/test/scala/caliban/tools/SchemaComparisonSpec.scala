package caliban.tools

import caliban.parsing.Parser
import caliban.tools.SchemaComparison.compareDocuments
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object SchemaComparisonSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SchemaComparisonSpec")(
      testM("field changed") {
        val schema1: String =
          """
          input HeroInput {
            test: String!
          }
          
          type Hero {
            name(pad: Int!): String!
            nick: String!
            bday: Int
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test: String
          }
          
          type Hero {
            name(pad: Int!): String!
            nick: String!
            bday: Int!
          }
            |""".stripMargin

        assertM(for {
          s1   <- Parser.parseQuery(schema1)
          s2   <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield diff.map(_.toString))(
          equalTo(
            List(
              "Type was changed from 'String!' to 'String' on field 'test' of type 'HeroInput'.",
              "Type was changed from 'Int' to 'Int!' on field 'bday' of type 'Hero'."
            )
          )
        )
      }
    )
}
