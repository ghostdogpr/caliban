package caliban.interop.tapir

import caliban.Macros.gqldoc
import sttp.tapir._
import zio.ZIO
import zio.test._
import zio.query.ZQuery

object TapirSpec extends ZIOSpecDefault {

  case class Book(title: String, year: Int)

  val titleParameter: EndpointInput[String] =
    query[String]("title").description("The title of the book")

  val getBook: PublicEndpoint[(String, String), String, String, Any] =
    endpoint.get
      .errorOut(stringBody)
      .in("book")
      .in(titleParameter)
      .in(header[String]("X-Auth-Token").description("The token is 'secret'"))
      .out(stringBody)

  override def spec =
    suite("TapirSpec")(
      test("test simple endpoint") {
        val api         = getBook.toGraphQL[Any] { case (title, token) => ZIO.succeed(s"$title+$token") }
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              book(title: "Title", X_Auth_Token: "token")
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"book":"Title+token"}""")
        }
      },
      test("test simple endpoint with ZQuery") {
        val api         = getBook.toGraphQLQuery { case (title, token) => ZQuery.succeed(s"$title+$token") }
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              book(title: "Title", X_Auth_Token: "token")
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"book":"Title+token"}""")
        }
      },
      test("test override operation name") {
        val api         = getBook
          .name("overRide with IllEgal-ChaRs !@()[]/,>")
          .toGraphQLQuery { case (title, token) => ZQuery.succeed(s"$title+$token") }
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              overRide_with_IllEgal_ChaRs_(title: "Title", X_Auth_Token: "token")
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"overRide_with_IllEgal_ChaRs_":"Title+token"}""")
        }
      }
    )
}
