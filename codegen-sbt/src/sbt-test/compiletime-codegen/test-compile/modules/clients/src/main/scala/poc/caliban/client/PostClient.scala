package poc.caliban.client

import poc.caliban.client.generated.posts.CalibanClient
import poc.caliban.client.generated.posts.CalibanClient._
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import zio.Task

trait PostClient {
  def postById(id: String): Task[Option[(String, String)]]
}

final class PostClientLive(backend: SttpBackend[Task, ZioStreams with WebSockets]) extends PostClient {
  import sttp.client3._

  private val serverUrl = uri"http://localhost:8088/api/graphql"

  /**
   * Jules' comment:
   *
   * In real-world code, I'd never accept to get a String and return 2 String. I'd use proper types but it's not important in this POC.
   */
  override def postById(id: String): Task[Option[(String, String)]] =
    CalibanClient.Query
      .postById(id)(Post.id(PostId.id) ~ Post.author(AuthorName.name))
      .toRequest(serverUrl)
      .send(backend)
      .map(_.body)
      .absolve

}
