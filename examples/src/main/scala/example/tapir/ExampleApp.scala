package example.tapir

import example.tapir.Endpoints._

import caliban.interop.tapir._
import caliban.{ GraphQL, Http4sAdapter }

import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import sttp.tapir.server.ServerEndpoint
import zio._
import zio.blocking.Blocking
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.ExecutionContext

object ExampleApp extends CatsApp {

  // approach 1: using `Endpoint` and providing the logic
  val graphql: GraphQL[Any] =
    addBook.toGraphQL((bookAddLogic _).tupled) |+|
      deleteBook.toGraphQL((bookDeleteLogic _).tupled) |+|
      booksListing.toGraphQL((bookListingLogic _).tupled)

  // approach 2: using the `ServerEndpoint` where logic is already provided
  type MyIO[+A] = IO[String, A]

  val addBookEndpoint: ServerEndpoint[(Book, String), String, Unit, Any, MyIO] =
    addBook.serverLogic[MyIO] { case (book, token) => bookAddLogic(book, token).either }
  val deleteBookEndpoint: ServerEndpoint[(String, String), String, Unit, Any, MyIO] =
    deleteBook.serverLogic[MyIO] { case (title, token) => bookDeleteLogic(title, token).either }
  val booksListingEndpoint: ServerEndpoint[(Option[Int], Option[Int]), Nothing, List[Book], Any, UIO] =
    booksListing.serverLogic[UIO] { case (year, limit) => bookListingLogic(year, limit).map(Right(_)) }

  val graphql2: GraphQL[Any] =
    addBookEndpoint.toGraphQL |+| deleteBookEndpoint.toGraphQL |+| booksListingEndpoint.toGraphQL

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
      interpreter <- graphql.interpreter
      _ <- BlazeServerBuilder[Task](ExecutionContext.global)
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
    } yield ()).exitCode
}
