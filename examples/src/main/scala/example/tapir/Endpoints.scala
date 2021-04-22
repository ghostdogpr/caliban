package example.tapir

import io.circe.generic.auto._
import sttp.tapir._
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import zio.{ IO, UIO }

object Endpoints {

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

  val baseEndpoint: Endpoint[Unit, String, Unit, Any] = endpoint.errorOut(stringBody).in("books")

  // POST /books
  val addBook: Endpoint[(Book, String), String, Unit, Any] =
    baseEndpoint.post
      .in("add")
      .in(
        jsonBody[Book]
          .description("The book to add")
          .example(Book("Pride and Prejudice", 1813))
      )
      .in(header[String]("X-Auth-Token").description("The token is 'secret'"))

  val titleParameter: EndpointInput[String] =
    query[String]("title").description("The title of the book")

  // DELETE /books
  val deleteBook: Endpoint[(String, String), String, Unit, Any] =
    baseEndpoint.delete
      .in("delete")
      .in(titleParameter)
      .in(header[String]("X-Auth-Token").description("The token is 'secret'"))

  // Re-usable parameter description
  val yearParameter: EndpointInput[Option[Int]] =
    query[Option[Int]]("year").description("The year from which to retrieve books")
  val limitParameter: EndpointInput[Option[Int]] =
    query[Option[Int]]("limit").description("Maximum number of books to retrieve")

  // GET /books
  val booksListing: Endpoint[(Option[Int], Option[Int]), Nothing, List[Book], Any] =
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

  def bookDeleteLogic(title: String, token: String): IO[String, Unit] =
    if (token != "secret") {
      IO.fail("Unauthorized access!!!11")
    } else {
      books = books.filterNot(_.title == title)
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
}
