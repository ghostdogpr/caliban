package example.gateway

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.ZHttpAdapter
import caliban.gateway.{ SubGraph, SuperGraph }
import caliban.interop.tapir.HttpInterpreter
import caliban.introspection.adt.TypeVisitor
import caliban.tools.SttpClient
import sttp.client3.httpclient.zio.HttpClientZioBackend
import sttp.tapir.json.circe._
import zio._
import zio.http._

object Gateway extends ZIOAppDefault {
  val stores: SubGraph[SttpClient]  = SubGraph.graphQL("Stores", "http://localhost:8081/api/graphql")
  val books: SubGraph[SttpClient]   = SubGraph.graphQL("Books", "http://localhost:8082/api/graphql")
  val authors: SubGraph[SttpClient] = SubGraph.graphQL("Authors", "http://localhost:8083/api/graphql")

  val gateway: SuperGraph[SttpClient] =
    SuperGraph
      .compose(List(stores, books, authors))
      .transform(TypeVisitor.filterField { case ("Query", fieldName) => fieldName == "stores" })
      .transform(TypeVisitor.renameType { case "authors_v1_Author" => "Author" })
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
        filterBatchResults = _.get("authorId") == _.get("id")
      )

  def run: Task[Unit] =
    gateway.build
      .tap(api => ZIO.debug(api.render))
      .flatMap(_.interpreter)
      .flatMap(interpreter =>
        Server.serve(
          Http.collectHttp[Request] { case _ -> Root / "api" / "graphql" =>
            ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
          }
        )
      )
      .provide(ZLayer.succeed(Server.Config.default.port(8084)), Server.live, HttpClientZioBackend.layer())
}
