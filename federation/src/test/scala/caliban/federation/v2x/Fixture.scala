package caliban.federation.v2x

import caliban.federation.EntityResolver
import caliban.{ graphQL, GraphQL, RootResolver }
import caliban.schema.{ ArgBuilder, Extended, Schema }
import zio.{ Random, UIO, ZIO }

import java.util.UUID

private[v2x] object Fixture {
  import caliban.federation.v2_3._

  @GQLKey("id")
  @GQLShareable
  case class User(
    id: UUID
  )

  case class Query(
    hello: String,
    user: User
  )

  implicit val userSchema: Schema[Any, User]   = Schema.gen
  implicit val querySchema: Schema[Any, Query] = Schema.gen

  val api = graphQL(
    RootResolver(
      Query(hello = "Hello World!", user = User(UUID.randomUUID()))
    )
  )
}

private[v2x] object Fixture2_12 {
  import caliban.federation.v2_12._

  @GQLKey("id")
  @GQLCacheTag(format = "user-{$key.id}")
  case class User(
    id: String
  )

  case class Post(
    id: UUID,
    author: User
  )

  case class PostsByUserArgs(id: UUID)

  case class UserByIdArgs(id: String)

  case class Query(
    @GQLCacheTag("profile") user: UIO[Extended[User]],
    @GQLCacheTag("users-list") users: UIO[List[User]],
    @GQLCacheTag("posts-user-{$args.userId}") postsByUser: PostsByUserArgs => UIO[List[Post]]
  )

  implicit val userSchema: Schema[Any, User]                      = Schema.gen
  implicit val userByIdArgsSchema: Schema[Any, UserByIdArgs]      = Schema.gen
  implicit val postSchema: Schema[Any, Post]                      = Schema.gen
  implicit val postsByUserSchema: Schema[Any, PostsByUserArgs]    = Schema.gen
  implicit val userByIdArgBuilder: ArgBuilder[UserByIdArgs]       = ArgBuilder.gen
  implicit val postsByUserArgBuilder: ArgBuilder[PostsByUserArgs] = ArgBuilder.gen
  implicit val querySchema: Schema[Any, Query]                    = Schema.gen

  private val randomUser             = Random.nextUUID.map(_.toString).map(User.apply)
  private def randomPost(user: User) = Random.nextUUID.map(Post(_, user))

  val userMap = List(User("1"), User("2")).map(u => u.id -> u).toMap

  val api = graphQL(
    RootResolver(
      Query(
        user = randomUser.map(cacheField(_)(List("top-level-user"))),
        users = ZIO.collectAll(List.fill(3)(randomUser)),
        postsByUser = _ => randomUser.flatMap(user => ZIO.collectAll(List.fill(3)(randomPost(user))))
      )
    )
  )

  def buildApi(resolver: EntityResolver[Any]): GraphQL[Any] =
    api @@ federated(resolver)
}
