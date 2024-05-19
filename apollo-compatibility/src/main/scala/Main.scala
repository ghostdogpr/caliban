import caliban.CalibanError
import zio._
import caliban.quick._
import services.{ InventoryService, ProductService, UserService }
import zio.http.{ Response, Routes, Server }

object Main extends ZIOAppDefault {

  def run = for {
    args <- ZIOAppArgs.getArgs
    _    <- (args match {
              case Chunk("printSchema") => printSchema
              case _                    => runServer
            })
  } yield ()

  private val printSchema = Console.printLine(ProductSchema.print)

  private val runServer = {
    val routes: Task[Routes[ProductService with UserService with InventoryService, Response]] =
      ProductSchema.api.routes("/graphql")

    val server: ZIO[ProductService with UserService with InventoryService with Server, Throwable, Response] =
      routes.flatMap(Server.serve(_))

    server.orDie
      .provide(
        Server.defaultWithPort(4001),
        ProductService.inMemory,
        UserService.inMemory,
        InventoryService.inMemory
      )
  }

}
