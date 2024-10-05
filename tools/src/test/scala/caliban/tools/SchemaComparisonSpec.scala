package caliban.tools

import caliban._
import caliban.parsing.Parser
import caliban.schema.Annotations.GQLDeprecated
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.tools.SchemaComparison.compareDocuments
import caliban.tools.SchemaComparisonChange.Target
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object SchemaComparisonSpec extends ZIOSpecDefault {

  def compare(
    schema1: String,
    schema2: String,
    expected: List[String]
  ): Either[CalibanError.ParsingError, TestResult] =
    for {
      s1  <- Parser.parseQuery(schema1)
      s2  <- Parser.parseQuery(schema2)
      diff = compareDocuments(s1, s2)
    } yield assertTrue(diff.map(_.toString) == expected)

  def compareChanges(
    schema1: String,
    schema2: String,
    expected: List[SchemaComparisonChange]
  ): Either[CalibanError.ParsingError, TestResult] =
    for {
      s1  <- Parser.parseQuery(schema1)
      s2  <- Parser.parseQuery(schema2)
      diff = compareDocuments(s1, s2)
    } yield assertTrue(diff == expected)

  override def spec =
    suite("SchemaComparisonSpec")(
      test("field changed") {
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
      test("type added") {
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
      test("description changes") {
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
      test("various changes") {
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
      test("optional argument added") {
        val schema1: String =
          """
          input HeroInput {
            test: String
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test: String
            test2: String
          }
            |""".stripMargin

        for {
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield assert(diff)(hasFirst(hasField("breaking", _.breaking, equalTo(false))))
      },
      test("non-optional argument added") {
        val schema1: String =
          """
          input HeroInput {
            test: String
          }
            |""".stripMargin

        val schema2: String =
          """
          input HeroInput {
            test: String
            test2: String!
          }
            |""".stripMargin

        for {
          s1  <- Parser.parseQuery(schema1)
          s2  <- Parser.parseQuery(schema2)
          diff = compareDocuments(s1, s2)
        } yield assert(diff)(hasFirst(hasField("breaking", _.breaking, equalTo(true))))
      },
      test("deprecated") {
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
      test("compare Caliban schema with string schema") {
        val schema: String =
          """
          type Hero {
            name(pad: Int!): String!
            nick: String! @deprecated(reason: "some reason")
            bday: Int
          }

          type Query {
            hero: Hero!
          }
            |""".stripMargin

        case class NameArgs(pad: Int)
        case class Hero(name: NameArgs => String, @GQLDeprecated("some reason") nick: String, bday: Option[Int])
        case class Query(hero: Hero)

        val api = graphQL(RootResolver(Query(Hero(_ => "name", "nick", None))))

        for {
          diff <- SchemaComparison.compare(SchemaLoader.fromString(schema), SchemaLoader.fromCaliban(api))
        } yield assertTrue(diff == Nil)
      },
      suite("repeatable directive")(
        test("becomes repeatable") {
          val nonRepeatable = "directive @test on FIELD_DEFINITION"
          val repeatable    = "directive @test repeatable on FIELD_DEFINITION"

          val expected = SchemaComparisonChange.DirectiveDefinitionRepeatableChanged("test", from = false, to = true)

          compareChanges(nonRepeatable, repeatable, List(expected)).map(
            _ && assertTrue(!expected.breaking)
          )
        },
        test("becomes non-repeatable") {
          val repeatable    = "directive @test repeatable on FIELD_DEFINITION"
          val nonRepeatable = "directive @test on FIELD_DEFINITION"

          val expected = SchemaComparisonChange.DirectiveDefinitionRepeatableChanged("test", from = true, to = false)

          compareChanges(repeatable, nonRepeatable, List(expected)).map {
            _ && assertTrue(expected.breaking)
          }
        },
        test("changes in base interfaces") {
          val schema1: String =
            """interface A { x: Int }
              |interface B { x: Int }
              |
              |type C implements A { x: Int }
              |""".stripMargin

          val schema2: String =
            """interface A { x: Int }
              |interface B { x: Int }
              |
              |type C implements B { x: Int }
              |""".stripMargin

          val expected = List(
            SchemaComparisonChange.ObjectImplementsAdded("C", "B"),
            SchemaComparisonChange.ObjectImplementsDeleted("C", "A")
          )

          compareChanges(schema1, schema2, expected)
        }
      ),
      suite("type extensions")(
        test("scala extensions") {
          val schema1: String =
            """
              | extend scalar MyScalar
              |""".stripMargin
          val schema2: String =
            """
              | extend scalar MyScalar @deprecated
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.DirectiveAdded("deprecated", Target.Type("MyScalar"))
          )
          compareChanges(schema1, schema2, expected)
        },
        test("object type extensions") {
          val schema1: String =
            """
              | extend type MyType implements A @deprecated(reason: "abandoned") {
              |   a: Int
              | }
              |""".stripMargin
          val schema2: String =
            """
              | extend type MyType {
              |   b: Int
              | }
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.DirectiveDeleted("deprecated", Target.Type("MyType")),
            SchemaComparisonChange.ObjectImplementsDeleted("MyType", "A"),
            SchemaComparisonChange.FieldAdded("MyType", "b"),
            SchemaComparisonChange.FieldDeleted("MyType", "a")
          )
          compareChanges(schema1, schema2, expected)
        },
        test("interface type extensions") {
          val schema1: String =
            """
              | extend interface MyInterface @deprecated(reason: "abandoned") {
              |   a: Int
              | }
              |""".stripMargin
          val schema2: String =
            """
              | extend interface MyInterface {
              |   b: Int
              | }
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.DirectiveDeleted("deprecated", Target.Type("MyInterface")),
            SchemaComparisonChange.FieldAdded("MyInterface", "b"),
            SchemaComparisonChange.FieldDeleted("MyInterface", "a")
          )
          compareChanges(schema1, schema2, expected)
        },
        test("union type extensions") {
          val schema1: String =
            """
              | extend union MyUnion = A | B
              |""".stripMargin
          val schema2: String =
            """
              | extend union MyUnion = A | C
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.UnionMemberAdded("MyUnion", "C"),
            SchemaComparisonChange.UnionMemberDeleted("MyUnion", "B")
          )
          compareChanges(schema1, schema2, expected)
        },
        test("enum type extensions") {
          val schema1: String =
            """
              | extend enum MyEnum { A B }
              |""".stripMargin
          val schema2: String =
            """
              | extend enum MyEnum { A C }
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.EnumValueAdded("MyEnum", "C"),
            SchemaComparisonChange.EnumValueDeleted("MyEnum", "B")
          )
          compareChanges(schema1, schema2, expected)
        },
        test("input type extensions") {
          val schema1: String =
            """
              | extend input MyInput {
              |   a: Int
              | }
              |""".stripMargin
          val schema2: String =
            """
              | extend input MyInput {
              |   b: Int
              | }
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.ArgumentAdded("b", Target.Type("MyInput"), true),
            SchemaComparisonChange.ArgumentDeleted("a", Target.Type("MyInput"))
          )
          compareChanges(schema1, schema2, expected)
        },
        test("type extensions added and deleted") {
          val schema1: String =
            """
              | extend scalar MyScalarA
              | extend scalar MyScalarB
              |""".stripMargin
          val schema2: String =
            """
              | extend scalar MyScalarA
              | extend scalar MyScalarC
              |""".stripMargin
          val expected        = List(
            SchemaComparisonChange.TypeExtensionAdded(name = "MyScalarC"),
            SchemaComparisonChange.TypeExtensionDeleted(name = "MyScalarB")
          )
          compareChanges(schema1, schema2, expected)
        }
      )
    )
}
