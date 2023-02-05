package example.tapir

import example.tapir.Endpoints._
import caliban.interop.tapir._
import caliban.{ GraphQL, Http4sAdapter }
import caliban.schema.auto._
import caliban.schema.ArgBuilder.auto._
import cats.data.Kleisli
import com.comcast.ip4s._
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import sttp.tapir.server.ServerEndpoint
import zio._
import zio.interop.catz._

object ExampleApp extends CatsApp {
  import sttp.tapir.json.circe._

  // approach 1: using `Endpoint` and providing the logic
  val graphql: GraphQL[Any] =
    addBook.toGraphQL((bookAddLogic _).tupled) |+|
      deleteBook.toGraphQL((bookDeleteLogic _).tupled) |+|
      booksListing.toGraphQL((bookListingLogic _).tupled)

  // approach 2: using the `ServerEndpoint` where logic is already provided
  type MyIO[+A] = IO[String, A]

  val addBookEndpoint: ServerEndpoint.Full[Unit, Unit, (Book, String), String, Unit, Any, MyIO]                        =
    addBook.serverLogic[MyIO] { case (book, token) => bookAddLogic(book, token).either }
  val deleteBookEndpoint: ServerEndpoint.Full[Unit, Unit, (String, String), String, Unit, Any, MyIO]                   =
    deleteBook.serverLogic[MyIO] { case (title, token) => bookDeleteLogic(title, token).either }
  val booksListingEndpoint: ServerEndpoint.Full[Unit, Unit, (Option[Int], Option[Int]), Nothing, List[Book], Any, UIO] =
    booksListing.serverLogic[UIO] { case (year, limit) => bookListingLogic(year, limit).map(Right(_)) }

  val graphql2: GraphQL[Any] =
    addBookEndpoint.toGraphQL |+| deleteBookEndpoint.toGraphQL |+| booksListingEndpoint.toGraphQL

  type MyTask[A] = Task[A]

  override def run =
    (for {
      interpreter <- graphql.interpreter
      _           <- EmberServerBuilder
                       .default[MyTask]
                       .withHost(host"localhost")
                       .withPort(port"8088")
                       .withHttpApp(
                         Router[MyTask](
                           "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(interpreter)),
                           "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                         ).orNotFound
                       )
                       .build
                       .toScopedZIO
                       .forever
    } yield ()).exitCode
}
