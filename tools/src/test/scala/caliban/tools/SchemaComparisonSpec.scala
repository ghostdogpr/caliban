package caliban.tools

import caliban.GraphQL.graphQL
import caliban.RootResolver
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
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield diff.map(_.toString))(
          equalTo(
            List(
              "Type was changed from 'String!' to 'String' on field 'test' of type 'HeroInput'.",
              "Type was changed from 'Int' to 'Int!' on field 'bday' of type 'Hero'."
            )
          )
        )
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

        assertM(for {
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield diff.map(_.toString))(equalTo(List("Type 'HeroInput' was added.")))
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

        assertM(for {
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield diff.map(_.toString))(
          equalTo(
            List(
              "Argument 'test2' was added on type 'HeroInput'.",
              "Argument 'test' was deleted from type 'HeroInput'.",
              "Type was changed from 'String!' to 'String' on field 'name' of type 'Hero'.",
              "Argument 'a' was added on field 'name' of type 'Hero'."
            )
          )
        )
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

        assertM(for {
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield diff.map(_.toString))(
          equalTo(List("Directive 'deprecated' was deleted from field 'test' of type 'HeroInput'."))
        )
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
