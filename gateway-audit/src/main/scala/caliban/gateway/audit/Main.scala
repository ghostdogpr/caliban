package caliban.gateway.audit

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, Subgraph }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, JsonValueCodec }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import zio._
import zio.http._

object Main extends ZIOAppDefault {

  private val GatewayPort = 4000

  private[audit] final case class SubgraphInput(name: String, url: String, sdl: String)
  private[audit] implicit val subgraphInputsCodec: JsonValueCodec[List[SubgraphInput]] = JsonCodecMaker.make

  override def run: ZIO[ZIOAppArgs with Scope, Throwable, Unit] =
    for {
      args        <- ZIOAppArgs.getArgs
      suite       <- ZIO
                       .fromOption(Some(args).collect { case Chunk(suite) => suite })
                       .orElseFail(new IllegalArgumentException("Expected one Federation audit suite id."))
      inputs      <- fetchSubgraphs(suite).provide(Client.default)
      subgraphs   <- ZIO.foreach(inputs)(input =>
                       ZIO.fromEither(URL.decode(input.url)).map(Subgraph.federation(input.name, _, input.sdl))
                     )
      interpreter <- Gateway.compose(subgraphs.head, subgraphs.tail: _*).interpreter
      _           <- ZIO.logInfo(s"Serving Federation audit suite '$suite' on port $GatewayPort.")
      _           <- QuickAdapter(interpreter).runServer(GatewayPort, "/graphql")
    } yield ()

  private def fetchSubgraphs(suite: String): ZIO[Client, Throwable, NonEmptyChunk[SubgraphInput]] =
    for {
      response <- Client.batched(Request.get(url"http://127.0.0.1:4200" / suite / "subgraphs"))
      _        <- ZIO
                    .fail(new IllegalStateException(s"Audit fixture request failed with HTTP ${response.status.code}."))
                    .unless(response.status.isSuccess)
      body     <- response.body.asArray
      inputs   <- ZIO.attempt(readFromArray[List[SubgraphInput]](body))
      nonEmpty <- ZIO
                    .fromOption(NonEmptyChunk.fromIterableOption(inputs))
                    .orElseFail(new IllegalArgumentException("Audit fixture returned no subgraphs."))
    } yield nonEmpty
}
