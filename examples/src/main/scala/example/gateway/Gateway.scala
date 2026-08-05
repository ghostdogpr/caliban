package example.gateway

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.gateway.{ SubGraph, SuperGraph }
import caliban.quick.GraphqlServerOps
import caliban.transformers.Transformer
import sttp.client4.Backend
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio._

object Gateway extends ZIOAppDefault {
  val stores: SubGraph[Backend[Task]]  = SubGraph.graphQL("Stores", "http://localhost:8081/api/graphql")
  val books: SubGraph[Backend[Task]]   = SubGraph.graphQL("Books", "http://localhost:8082/api/graphql")
  val authors: SubGraph[Backend[Task]] = SubGraph.graphQL("Authors", "http://localhost:8083/api/graphql")

  val gateway: SuperGraph[Backend[Task]] =
    SuperGraph
      .compose(List(stores, books, authors))
      .transform(Transformer.ExcludeField.when { case ("Query", fieldName) => fieldName != "stores" })
      .transform(Transformer.RenameType("authors_v1_Author" -> "Author"))
      .extend(
        stores,
        sourceFieldName = "bookSells",
        targetTypeName = "Store",
        targetFieldName = "bookSells",
        argumentMappings = Map("id" -> ("storeId" -> _))
      )
      .extend(
        books,
        sourceFieldName = "book",
        targetTypeName = "Sells",
        targetFieldName = "book",
        argumentMappings = Map("bookId" -> ("id" -> _))
      )
      .extend(
        authors,
        sourceFieldName = "authors_v1_AuthorsService_GetAuthors",
        targetTypeName = "Book",
        targetFieldName = "author",
        argumentMappings = Map("authorId" -> (v => "input" -> ObjectValue(Map("ids" -> ListValue(List(v)))))),
        filterBatchResults = Some(_.get("authorId") == _.get("id")),
        additionalFields = List("id")
      )

  def run: Task[Unit] =
    gateway.build
      .tap(api => ZIO.debug(api.render))
      .flatMap(_.runServer(8084, apiPath = "api/graphql"))
      .provide(HttpClientZioBackend.layer())
}
