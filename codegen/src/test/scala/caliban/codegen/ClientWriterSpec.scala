package caliban.codegen

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion._
import zio.test.{ assertM, suite, testM, DefaultRunnableSpec }

object ClientWriterSpec
    extends DefaultRunnableSpec(
      {
        val gen: String => Task[String] = (schema: String) =>
          Parser
            .parseQuery(schema)
            .flatMap(doc => Formatter.formatStr(ClientWriter.write(doc), CodegenPlugin.scalafmtConfig))

        suite("ClientWriterSpec")(
          testM("simple object type") {
            val schema =
              """
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

            assertM(
              gen(schema),
              equalTo(
                """object Data {

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
              )
            )
          },
          testM("nested object type") {
            val schema =
              """
             type Query {
               characters: [Character!]!
             }
             
             type Character {
               name: String!
               nicknames: [String!]!
             }
            """.stripMargin

            assertM(
              gen(schema),
              equalTo(
                """object Data {

  type Query
  object Query {
    def characters[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Query, List[A]] =
      Field("characters", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
  }

}
"""
              )
            )
          }
        )
      }
    )
