package caliban.modelgen
import caliban.modelgen.Generator.RootQueryDef
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import caliban.parsing.adt.ExecutableDefinition.{FragmentDefinition, TypeDefinition}
import zio.test.Assertion._
import zio.test._
import zio._
object GeneratorSpec extends DefaultRunnableSpec(
  suite("Generator single values")(
    testM("simple type") {
      val gqltype =
        """
          type Hero {
                name(pad: Int!): String! @skip(if: $someTestM)
                nick: String!
                bday: Int
              }
          |""".stripMargin

      implicit val writer = ScalaWriter.DefaultGQLWriter

      val caseclassstrdef = Parser.parseQuery(gqltype).map(doc => {
        Document.typeDefinitions(doc).map(ScalaWriter.TypeDefinitionWriter.write(_)(doc)).mkString("\n")
      })

      assertM(
        caseclassstrdef,
        equalTo(
          "case class Hero(name: String, nick: String, bday: Option[Int])"
        )
      )
    },
    testM("simple fragment") {
      val query =
      """
         type User {
           id: Int
           name: String
           profilePic: String
         }
         fragment friendFields on User {
           id
           name
           profilePic(size: 50)
         }"""

      implicit val writer = ScalaWriter.DefaultGQLWriter

      val caseclassstrdef = Parser.parseQuery(query).map(doc => {
        Document.fragmentDefinitions(doc).map( d => ScalaWriter.FragmentWriter.write(d)(doc)).mkString("\n")
      })

      assertM(
        caseclassstrdef,
        equalTo(
          """case class friendFields(id: Option[Int], name: Option[String], profilePic: Option[String])""".stripMargin
        )
      )
    },
    testM("simple query") {
      val query =
        """
         type Query {
           user(id: Int): User
         }
         type User {
           id: Int
           name: String
           profilePic: String
         }"""
      implicit val writer = ScalaWriter.DefaultGQLWriter

      val caseclassstrdef = Parser.parseQuery(query).map(doc => {
        Document.typeDefinition("Query")(doc).map( d => ScalaWriter.RootQueryDefWriter.write(RootQueryDef(d))(doc)).mkString("\n")
      })

      assertM(
        caseclassstrdef,
        equalTo(
          """
            |case class userArgs(id: Option[Int])
            |case class Queries(
            |user: userArgs => Option[User]
            |)""".stripMargin
        )
      )
    },
  )
)
