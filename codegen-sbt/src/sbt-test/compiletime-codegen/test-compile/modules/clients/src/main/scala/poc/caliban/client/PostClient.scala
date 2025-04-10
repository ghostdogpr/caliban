package poc.caliban.client

import poc.caliban.client.generated.posts.CalibanClient
import poc.caliban.client.generated.posts.CalibanClient._
import sttp.client4.Backend
import zio.Task

trait PostClient {
  def postById(id: String): Task[Option[(String, String)]]
}

final class PostClientLive(backend: Backend[Task]) extends PostClient {
  import sttp.client4._

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
