package example.calibantotapir.graphql

import caliban.schema.ArgBuilder
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import caliban.schema.Schema.auto._
import zio._

final case class Book(title: String, year: Int)
object Book {
  implicit val bookSchema: Schema[Any, Book] = Schema.gen
}

final case class BookSearchArgs(year: Option[Int], limit: Option[Int])
object BookSearchArgs {
  implicit val bookSearchArgsSchema: ArgBuilder[BookSearchArgs] = ArgBuilder.gen
}

class BookRepository(books: Ref[List[Book]]) {
  def add(book: Book): UIO[Unit] = books.update(books => book :: books)

  def delete(title: String): UIO[Unit] = books.update(_.filterNot(_.title == title))

  def list(year: Option[Int], limit: Option[Int]): UIO[List[Book]] = {
    val lim       = limit.getOrElse(Int.MaxValue)
    val yearMatch = (incomingYear: Int) => year.map(_ == incomingYear).getOrElse(true)
    books.get.map(_.filter(book => yearMatch(book.year)).take(lim))
  }
}
object BookRepository                        {
  val books = List(
    Book("The Sorrows of Young Werther", 1774),
    Book("Iliad", -8000),
    Book("Nad Niemnem", 1888),
    Book("The Colour of Magic", 1983),
    Book("The Art of Computer Programming", 1968),
    Book("Pharaoh", 1897),
    Book("Lords and Ladies", 1992)
  )

  val layer: ULayer[BookRepository] = ZLayer(Ref.make[List[Book]](books).map(new BookRepository(_)))
}

final case class Queries(
  books: BookSearchArgs => UIO[List[Book]]
)
object Queries                               {
  implicit val queriesSchema: Schema[Any, Queries] = Schema.gen

  val layer: URLayer[BookRepository, Queries] = ZLayer {
    for {
      repo <- ZIO.service[BookRepository]
    } yield Queries(args => repo.list(args.year, args.limit))
  }
}

final case class Mutations(
  addBook: Book => UIO[Unit],
  deleteBook: String => UIO[Unit]
)
object Mutations                             {
  implicit val mutationsSchema: Schema[Any, Mutations] = Schema.gen

  val layer: URLayer[BookRepository, Mutations] = ZLayer {
    for {
      repo <- ZIO.service[BookRepository]
    } yield Mutations(repo.add, repo.delete)
  }
}
