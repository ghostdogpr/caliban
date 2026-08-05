package caliban.gateway

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.transformers.Transformer
import zio._
import zio.test._

object SuperGraphSpec extends ZIOSpecDefault {
  case class Book(authorId: String)
  case class Author(id: String, name: String)
  case class BooksQuery(books: List[Book])
  case class AuthorsArgs(ids: List[String])
  case class AuthorsQuery(authors: AuthorsArgs => List[Author])
  case class EchoArgs(input: String)
  case class EchoResult(value: String)
  case class EchoQuery(echo: EchoArgs => EchoResult)

  val booksApi: GraphQL[Any] =
    graphQL(RootResolver(BooksQuery(List(Book("author-1")))))

  val authorsApi: GraphQL[Any] =
    graphQL(
      RootResolver(
        AuthorsQuery(args =>
          List(Author("author-1", "Alice"), Author("author-2", "Bob")).filter(a => args.ids.contains(a.id))
        )
      )
    )

  def spec = suite("SuperGraphSpec")(
    test("fetches fields required to correlate batch results even when the client does not select them") {
      val books   = SubGraph.caliban("Books", booksApi)
      val authors = SubGraph.caliban("Authors", authorsApi, exposeAtRoot = false)

      for {
        api         <- SuperGraph
                         .compose(List(books, authors))
                         .transform(Transformer.RenameType("Author" -> "Writer"))
                         .extend(
                           authors,
                           sourceFieldName = "authors",
                           targetTypeName = "Book",
                           targetFieldName = "author",
                           argumentMappings = Map(
                             "authorId" -> (value => "ids" -> ListValue(List(value)))
                           ),
                           filterBatchResults = Some(_.get("authorId") == _.get("id")),
                           additionalFields = List("id")
                         )
                         .build
        interpreter <- api.interpreter
        response    <- interpreter.executeRequest(GraphQLRequest(Some("{ books { author { name } } }")))
      } yield assertTrue(
        response.errors.isEmpty,
        response.data.toString.contains("\"name\":\"Alice\""),
        api.render.contains("type Writer"),
        !api.render.contains("type Author")
      )
    },
    test("maps renamed fields and arguments back to their subgraph names") {
      val echo = SubGraph.caliban("Echo", graphQL(RootResolver(EchoQuery(args => EchoResult(args.input)))))

      for {
        api         <- SuperGraph
                         .compose(List(echo))
                         .transform(
                           Transformer.RenameField(
                             "EchoQuery"  -> "echo"  -> "renamedEcho",
                             "EchoResult" -> "value" -> "renamedValue"
                           )
                         )
                         .transform(
                           Transformer.RenameArgument(
                             "EchoQuery" -> "renamedEcho" -> "input" -> "renamedInput"
                           )
                         )
                         .build
        interpreter <- api.interpreter
        response    <- interpreter.executeRequest(
                         GraphQLRequest(Some("{ renamedEcho(renamedInput: \"hello\") { renamedValue } }"))
                       )
      } yield assertTrue(
        response.errors.isEmpty,
        response.data.toString.contains("\"renamedValue\":\"hello\"")
      )
    }
  )
}
