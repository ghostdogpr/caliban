package caliban.federation.v2x

import caliban.{ graphQL, RootResolver }
import caliban.federation.v2_3._
import caliban.schema.Schema

import java.util.UUID

private[v2x] object Fixture {

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
