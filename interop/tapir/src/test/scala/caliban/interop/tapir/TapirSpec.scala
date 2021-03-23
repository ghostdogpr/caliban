package caliban.interop.tapir

import caliban.Macros.gqldoc
import sttp.tapir._
import zio.UIO
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.query.ZQuery

object TapirSpec extends DefaultRunnableSpec {

  case class Book(title: String, year: Int)

  val titleParameter: EndpointInput[String] =
    query[String]("title").description("The title of the book")

  val getBook: Endpoint[(String, String), String, String, Any] =
    endpoint.get
      .errorOut(stringBody)
      .in("book")
      .in(titleParameter)
      .in(header[String]("X-Auth-Token").description("The token is 'secret'"))
      .out(stringBody)

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("TapirSpec")(
      testM("test simple endpoint") {
        val api         = getBook.toGraphQL[Any]({ case (title, token) => UIO(s"$title+$token") })
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              book(title: "Title", X_Auth_Token: "token")
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"book":"Title+token"}""")
        )
      },
      testM("test simple endpoint with ZQuery") {
        val api         = getBook.toGraphQLQuery({ case (title, token) => ZQuery.succeed(s"$title+$token") })
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              book(title: "Title", X_Auth_Token: "token")
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"book":"Title+token"}""")
        )
      },
      testM("test override operation name") {
        val api         = getBook
          .name("overRide with IllEgal-ChaRs !@()[]/,>")
          .toGraphQLQuery({ case (title, token) => ZQuery.succeed(s"$title+$token") })
        val interpreter = api.interpreter
        val query       = gqldoc("""
            query test {
              overRide_with_IllEgal_ChaRs_(title: "Title", X_Auth_Token: "token")
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"overRide_with_IllEgal_ChaRs_":"Title+token"}""")
        )
      }
    )
}
