package caliban.tools

import caliban.GraphQL.graphQL
import caliban.parsing.Parser
import caliban.tools.SchemaComparison.compareDocuments
import caliban.{ CalibanError, RootResolver }
import zio.ZIO
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object SchemaComparisonSpec extends DefaultRunnableSpec {

  def compare(
    schema1: String,
    schema2: String,
    expected: List[String]
  ): ZIO[Any, CalibanError.ParsingError, TestResult] =
    assertM(for {
      s1  <- Parser.parseQuery(schema1)
      s2  <- Parser.parseQuery(schema2)
      diff = compareDocuments(s1, s2)
    } yield diff.map(_.toString))(equalTo(expected))

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

        val expected = List(
          "Type was changed from 'String!' to 'String' on field 'test' of type 'HeroInput'.",
          "Type was changed from 'Int' to 'Int!' on field 'bday' of type 'Hero'."
        )

        compare(schema1, schema2, expected)
      },
      testM("type added") {
        val schema1: String =
          """
          type Hero {
            name(pad: Int!): String!
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test: String
          }
          
          type Hero {
            name(pad: Int!): String!
          }
            |""".stripMargin

        compare(schema1, schema2, List("Type 'HeroInput' was added."))
      },
      testM("description changes") {
        val schema1: String =
          """
          type DescTest {
            add: String
            "change X"
            change: String
            "remove"
            remove: String
          }
            |""".stripMargin

        val schema2: String =
          """
          type DescTest {
            "added desc"
            add: String
            "changed to Y"
            change: String
            remove: String
          }
            |""".stripMargin

        val expected = List(
          "Description was added on field 'add' of type 'DescTest'.",
          "Description was changed on field 'change' of type 'DescTest'.",
          "Description was deleted on field 'remove' of type 'DescTest'."
        )
        compare(schema1, schema2, expected)
      },
      testM("various changes") {
        val schema1: String =
          """
          input HeroInput {
            test: String
          }
          
          type Hero {
            name(pad: Int!): String!
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test2: String
          }
          
          type Hero {
            name(pad: Int!, a: String): String
          }
            |""".stripMargin

        val expected = List(
          "Argument 'test2' was added on type 'HeroInput'.",
          "Argument 'test' was deleted from type 'HeroInput'.",
          "Type was changed from 'String!' to 'String' on field 'name' of type 'Hero'.",
          "Argument 'a' was added on field 'name' of type 'Hero'."
        )

        compare(schema1, schema2, expected)
      },
      testM("deprecated") {
        val schema1: String =
          """
          input HeroInput {
            test: String @deprecated
          }
          
          type Hero {
            name(pad: Int!): String!
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test: String
          }
          
          type Hero {
            name(pad: Int!): String!
          }
            |""".stripMargin

        compare(schema1, schema2, List("Directive 'deprecated' was deleted from field 'test' of type 'HeroInput'."))
      },
      testM("compare Caliban schema with string schema") {
        val schema: String =
          """
          type Hero {
            name(pad: Int!): String!
            nick: String!
            bday: Int
          }
          
          type Query {
            hero: Hero!
          }
            |""".stripMargin

        case class NameArgs(pad: Int)
        case class Hero(name: NameArgs => String, nick: String, bday: Option[Int])
        case class Query(hero: Hero)

        val api = graphQL(RootResolver(Query(Hero(_ => "name", "nick", None))))

        assertM(for {
          diff <- SchemaComparison.compare(SchemaLoader.fromString(schema), SchemaLoader.fromCaliban(api))
        } yield diff)(equalTo(Nil))
      }
    )
}
