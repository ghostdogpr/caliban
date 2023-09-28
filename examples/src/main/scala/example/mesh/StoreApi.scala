package example.mesh

import caliban.Value.StringValue
import caliban._
import caliban.interop.tapir.HttpInterpreter
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.schema.{ ArgBuilder, Schema }
import sttp.tapir.json.circe._
import zio._
import zio.http._

object StoreApi extends ZIOAppDefault {
  case class ID(value: String)
  case class Store(id: ID, name: String, location: String)
  case class BookSellArgs(storeId: ID)
  case class Sells(bookId: ID, sellsCount: Int, monthYear: String, storeId: ID)
  case class Query(stores: List[Store], bookSells: BookSellArgs => List[Sells])

  val stores = List(
    Store(ID("0"), "Librairie", "Paris, France"),
    Store(ID("1"), "Book store", "New York, New York, United States")
  )
  val sells  = List(
    Sells(ID("0"), 1932, "03/22", ID("0")),
    Sells(ID("1"), 192, "03/22", ID("0")),
    Sells(ID("0"), 2387, "03/22", ID("1")),
    Sells(ID("1"), 283, "03/22", ID("1"))
  )

  implicit val idSchema: Schema[Any, ID]    = Schema.scalarSchema("ID", None, None, None, id => StringValue(id.value))
  implicit val idArgBuilder: ArgBuilder[ID] = ArgBuilder.string.map(ID)

  val resolver = RootResolver(Query(stores, args => sells.filter(_.storeId == args.storeId)))
  val api      = graphQL(resolver)

  def run: Task[Unit] =
    api.interpreter
      .flatMap(interpreter =>
        Server.serve(
          Http.collectHttp[Request] { case _ -> Root / "api" / "graphql" =>
            ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
          }
        )
      )
      .provide(ZLayer.succeed(Server.Config.default.port(8081)), Server.live)
}
