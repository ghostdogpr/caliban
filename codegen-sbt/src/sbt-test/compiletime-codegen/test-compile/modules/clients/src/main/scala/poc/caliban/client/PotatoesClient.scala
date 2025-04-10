package poc.caliban.client

import poc.caliban.client.generated.potatoes._
import sttp.client4.Backend
import zio.{ Task, ZIO }

trait PotatoesClient {
  def eradicate(name: String): Task[Unit]
}

final class PotatoesClientLive(backend: Backend[Task]) extends PotatoesClient {
  import sttp.client4._

  private val serverUrl = uri"http://localhost:8088/api/graphql"

  override def eradicate(name: String): Task[Unit] =
    Mutation
      .eradicate(name)
      .toRequest(serverUrl)
      .send(backend)
      .foldZIO(ZIO.fail(_), r => ZIO.fromEither(r.body).unit)

}
