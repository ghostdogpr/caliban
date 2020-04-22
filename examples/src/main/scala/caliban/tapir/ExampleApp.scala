package caliban.tapir

import caliban.{ GraphQL, Http4sAdapter }
import caliban.interop.tapir._
import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.console.putStrLn
import zio.interop.catz._
import zio.interop.catz.implicits._

object ExampleApp extends CatsApp {

  import io.circe.generic.auto._
  import sttp.tapir._
  import sttp.tapir.json.circe._

  case class Book(title: String, year: Int)

  var books = List(
    Book("The Sorrows of Young Werther", 1774),
    Book("Iliad", -8000),
    Book("Nad Niemnem", 1888),
    Book("The Colour of Magic", 1983),
    Book("The Art of Computer Programming", 1968),
    Book("Pharaoh", 1897),
    Book("Lords and Ladies", 1992)
  )

  val baseEndpoint: Endpoint[Unit, String, Unit, Nothing] = endpoint.errorOut(stringBody).in("books")

  // POST /books
  val addBook: Endpoint[(Book, String), String, Unit, Nothing] =
    baseEndpoint.post
      .in("add")
      .in(
        jsonBody[Book]
          .description("The book to add")
          .example(Book("Pride and Prejudice", 1813))
      )
      .in(header[String]("X-Auth-Token").description("The token is 'secret'"))

  // Re-usable parameter description
  val yearParameter: EndpointInput[Option[Int]] =
    query[Option[Int]]("year").description("The year from which to retrieve books")
  val limitParameter: EndpointInput[Option[Int]] =
    query[Option[Int]]("limit").description("Maximum number of books to retrieve")

  case class Params(year: Option[Int], limit: Option[Int])

  // GET /books
  val booksListing: Endpoint[(Option[Int], Option[Int]), Nothing, List[Book], Nothing] =
    infallibleEndpoint
      .in("books")
      .get
      .in(yearParameter)
      .in(limitParameter)
      .out(jsonBody[List[Book]])

  def bookAddLogic(book: Book, token: String): IO[String, Unit] =
    if (token != "secret") {
      IO.fail("Unauthorized access!!!11")
    } else {
      books = book :: books
      IO.unit
    }

  def bookListingLogic(year: Option[Int], limit: Option[Int]): UIO[List[Book]] =
    UIO {
      val filteredBooks = year match {
        case None    => books
        case Some(y) => books.filter(_.year == y)
      }
      val limitedBooks = limit match {
        case None    => filteredBooks
        case Some(l) => filteredBooks.take(l)
      }
      limitedBooks
    }

  val graphql: GraphQL[Any] =
    addBook.toGraphQL((bookAddLogic _).tupled) |+|
      booksListing.toGraphQL((bookListingLogic _).tupled)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
      interpreter <- graphql.interpreter
      _ <- BlazeServerBuilder[Task]
            .bindHttp(8088, "localhost")
            .withHttpApp(
              Router[Task](
                "/api/graphql" -> CORS(Http4sAdapter.makeHttpService(interpreter)),
                "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", blocker, None))
              ).orNotFound
            )
            .resource
            .toManaged
            .useForever
    } yield 0)
      .catchAll(err => putStrLn(err.toString).as(1))

}
