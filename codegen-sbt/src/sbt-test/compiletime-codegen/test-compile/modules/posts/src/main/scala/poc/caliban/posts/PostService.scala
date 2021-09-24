package poc.caliban.posts

import poc.caliban.posts.PostServiceError.SomethingWentWrong
import zio._
import zio.stream.ZStream

import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final case class PostId(id: UUID)
object PostId           {
  def newRandom: PostId = PostId(id = UUID.randomUUID())
}
final case class AuthorName(name: String)
final case class PostTitle(title: String)
final case class PostContent(content: String)

final case class Post(id: PostId, author: AuthorName, title: PostTitle, content: PostContent)

sealed trait PostServiceError extends Throwable
object PostServiceError {
  case object DbIsKo                                extends PostServiceError
  final case class SomethingWentWrong(e: Throwable) extends PostServiceError
}

trait PostService {
  def findById(id: PostId): IO[PostServiceError, Option[Post]]
  def createPost(author: AuthorName, title: PostTitle, content: PostContent): IO[PostServiceError, Post]
  def deletePost(id: PostId): IO[PostServiceError, Unit]
  def all: ZStream[Any, PostServiceError, Post]
}

object PostService          extends Accessible[PostService] {
  val layer: ULayer[Has[PostService]] = ZLayer.succeed(new PostServiceLive)
}

final class PostServiceLive extends PostService             {
  private val db: mutable.Map[PostId, Post] = TrieMap.empty[PostId, Post]

  override def findById(id: PostId): IO[PostServiceError, Option[Post]] = IO(db.get(id)).mapError(SomethingWentWrong)

  override def createPost(author: AuthorName, title: PostTitle, content: PostContent): IO[PostServiceError, Post] =
    IO {
      val newId   = PostId.newRandom
      val newPost = Post(id = newId, author = author, title = title, content = content)
      db.put(newId, newPost)
      newPost
    }.mapError(SomethingWentWrong)

  override def deletePost(id: PostId): IO[PostServiceError, Unit] =
    IO(db.remove(id)).unit.mapError(SomethingWentWrong)

  override def all: ZStream[Any, PostServiceError, Post] =
    ZStream.fromIterable(db.values)
}
