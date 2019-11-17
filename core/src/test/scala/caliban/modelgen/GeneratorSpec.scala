package caliban.modelgen
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import caliban.parsing.adt.ExecutableDefinition.TypeDefinition
import zio.test.Assertion._
import zio.test._
import zio._
object GeneratorSpec extends DefaultRunnableSpec(
  suite("Generator single values")(
    testM("simple query") {
      val gqltype =
        """
          type Hero {
                name(pad: Int!): String! @skip(if: $someTestM)
                nick: String!
                bday: Int
              }
          |""".stripMargin

      val caseclassstrdef = Parser.parseQuery(gqltype).map(doc => {
        Document.typeDefinitions(doc).map(Generator.caseClassFromType(_)).mkString("\n")
      })

      assertM(
        caseclassstrdef,
        equalTo(
          "case class Hero(name: String, nick: String, bday: Option[Int])"
        )
      )
    }
  )
)
