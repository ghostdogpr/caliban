package poc.caliban.client

import poc.caliban.client.generated.potatoes._
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import zio.{ Task, ZIO }

trait PotatoesClient {
  def eradicate(name: String): Task[Unit]
}

final class PotatoesClientLive(backend: SttpBackend[Task, ZioStreams with WebSockets]) extends PotatoesClient {
  import sttp.client3._

  private val serverUrl = uri"http://localhost:8088/api/graphql"

  override def eradicate(name: String): Task[Unit] =
    Mutation
      .eradicate(name)
      .toRequest(serverUrl)
      .send(backend)
      .foldZIO(ZIO.fail(_), r => ZIO.fromEither(r.body).unit)

}
