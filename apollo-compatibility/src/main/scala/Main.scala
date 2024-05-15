import caliban.CalibanError
import zio._
import caliban.quick._
import services.{ InventoryService, ProductService, UserService }
import zio.http.Server

object Main extends ZIOAppDefault {

  def run = for {
    args <- ZIOAppArgs.getArgs
    _    <- (args match {
              case Chunk("printSchema") => printSchema
              case _                    => runServer
            })
  } yield ()

  val printSchema = Console.printLine(ProductSchema.print)

  val runServer = {
    val server: ZIO[
      ProductService with UserService with InventoryService with Server,
      CalibanError.ValidationError,
      Int
    ] = (ProductSchema.api.toApp("/graphql") flatMap Server.serve)

    server.orDie
      .provide(
        Server.defaultWithPort(4001),
        ProductService.inMemory,
        UserService.inMemory,
        InventoryService.inMemory
      )
  }

}
