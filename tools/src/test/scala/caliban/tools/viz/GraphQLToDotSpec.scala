package caliban.tools
package viz
import zio.ZIO
import zio.test._
import caliban.parsing.Parser

object GraphQLToDotSpec extends SnapshotTest {
  override val testName: String = "GraphQLToDotSpec"
  val assertions                = List(snapshotTest("simple-schema", ".dot") {
    val simpleSchema = """
            type Root {
               account(id: String!): Account
               allAccounts: [Account!]!
               search(input: SearchInput!): [Account!]!
            }

            interface Node {
              id: String!
            }

            enum Status {
              ACTIVE
              INACTIVE
            }

            input SearchInput {
              text: String!
            }

            type Account implements & Node {
              id: String!
              email: String
              status: Status!
            }
            """
    ZIO.fromEither(Parser.parseQuery(simpleSchema)).map { doc =>
      GraphQLToDot.generate(doc)
    }
  })
  def spec                      = suite("GraphQLToDotSpec")(assertions: _*)
}
