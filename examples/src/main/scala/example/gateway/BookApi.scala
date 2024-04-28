package example.gateway

import caliban._
import caliban.interop.tapir.HttpInterpreter
import caliban.quick.GraphqlServerOps
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import sttp.tapir.json.circe._
import zio._
import zio.http._

object BookApi extends ZIOAppDefault {
  case class BookArgs(id: String)
  case class BooksArgs(limit: Int)
  case class CategoriesArgs(limit: Int)
  case class Book(authorId: String, categoryId: String, id: String, title: String)
  case class Category(id: String, name: String)
  case class Query(book: BookArgs => Book, books: BooksArgs => List[Book], categories: CategoriesArgs => List[Category])

  val categories = List(
    Category("0", "Fiction"),
    Category("1", "French")
  )
  val books      = List(
    Book("1", "1", "0", "Illusion Perdues"),
    Book("0", "0", "1", "Dune")
  )

  val resolver = RootResolver(
    Query(args => books.find(_.id == args.id).get, args => books.take(args.limit), args => categories.take(args.limit))
  )
  val api      = graphQL(resolver)

  def run: Task[Unit] =
    api.runServer(8082, "/api/graphql")
}
