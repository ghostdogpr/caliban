package poc.caliban.posts

import zio._
import zio.stream.ZStream

import java.util.UUID

final case class PostId(id: UUID)
final case class AuthorName(name: String)
final case class PostTitle(title: String)
final case class PostContent(content: String)

final case class Post(id: PostId, author: AuthorName, title: PostTitle, content: PostContent)

sealed trait PostServiceError extends Throwable
object PostServiceError {
  case object DbIsKo extends PostServiceError
}

trait PostService {
  def findById(id: PostId): IO[PostServiceError, Post]
  def createPost(author: AuthorName, title: PostTitle, content: PostContent): IO[PostServiceError, Post]
  def deletePost(id: PostId): IO[PostServiceError, Unit]
  def all: ZStream[Any, PostServiceError, Post]
}

object PostService {
  def findById(id: PostId)                                                   = ZIO.serviceWithZIO[PostService](_.findById(id))
  def createPost(author: AuthorName, title: PostTitle, content: PostContent) =
    ZIO.serviceWithZIO[PostService](_.createPost(author, title, content))
  def deletePost(id: PostId)                                                 = ZIO.serviceWithZIO[PostService](_.deletePost(id))
  def all                                                                    = ZStream.serviceWithStream[PostService](_.all)
}
