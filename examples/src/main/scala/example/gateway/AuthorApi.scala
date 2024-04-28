package example.gateway

import caliban.Value.StringValue
import caliban._
import caliban.interop.tapir.HttpInterpreter
import caliban.quick.GraphqlServerOps
import caliban.schema.{ ArgBuilder, Schema }
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import sttp.tapir.json.circe._
import zio._
import zio.http._

object AuthorApi extends ZIOAppDefault {

  sealed trait ConnectivityState
  object ConnectivityState {
    case object IDLE              extends ConnectivityState
    case object CONNECTING        extends ConnectivityState
    case object READY             extends ConnectivityState
    case object TRANSIENT_FAILURE extends ConnectivityState
    case object SHUTDOWN          extends ConnectivityState
  }
  case class authors_v1_Author(id: String, name: String, editor: String)
  case class authors_v1_GetAuthorRequest_Input(id: String)
  case class authors_v1_GetAuthorsRequest_Input(ids: List[String])
  case class authors_v1_ListAuthorsResponse(items: List[authors_v1_Author])
  case class authors_v1_ListAuthorsRequest_Input(value: String)

  implicit val idSchema: Schema[Any, authors_v1_ListAuthorsRequest_Input]    =
    Schema.scalarSchema("authors_v1_ListAuthorsRequest_Input", None, None, None, id => StringValue(id.value))
  implicit val idArgBuilder: ArgBuilder[authors_v1_ListAuthorsRequest_Input] =
    ArgBuilder.string.map(authors_v1_ListAuthorsRequest_Input)

  case class authors_v1_AuthorsService_GetAuthorArgs(input: authors_v1_GetAuthorRequest_Input)
  case class authors_v1_AuthorsService_GetAuthorsArgs(input: authors_v1_GetAuthorsRequest_Input)
  case class authors_v1_ListAuthorsRequest_InputArgs(input: authors_v1_ListAuthorsRequest_Input)
  case class authors_v1_AuthorsService_connectivityStateArgs(tryToConnect: Boolean)

  case class Query(
    authors_v1_AuthorsService_GetAuthor: authors_v1_AuthorsService_GetAuthorArgs => Option[authors_v1_Author],
    authors_v1_AuthorsService_GetAuthors: authors_v1_AuthorsService_GetAuthorsArgs => List[authors_v1_Author],
    authors_v1_AuthorsService_ListAuthors: authors_v1_ListAuthorsRequest_InputArgs => authors_v1_ListAuthorsResponse,
    authors_v1_AuthorsService_connectivityState: authors_v1_AuthorsService_connectivityStateArgs => ConnectivityState
  )

  val authors =
    List(
      authors_v1_Author("0", "Jean France", "Gallimard"),
      authors_v1_Author("1", "James State", "Pearson")
    )

  val resolver = RootResolver(
    Query(
      args => authors.find(_.id == args.input.id),
      args => authors.filter(author => args.input.ids.contains(author.id)),
      _ => authors_v1_ListAuthorsResponse(authors),
      _ => ConnectivityState.IDLE
    )
  )
  val api      = graphQL(resolver)

  def run: Task[Unit] =
    api.runServer(8083, "/api/graphql")
}
