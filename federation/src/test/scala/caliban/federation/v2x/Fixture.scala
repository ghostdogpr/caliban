package caliban.federation.v2x

import caliban.{ graphQL, RootResolver }
import caliban.schema.Schema.auto._
import caliban.federation.v2_3._

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

  val api = graphQL(
    RootResolver(
      Query(hello = "Hello World!", user = User(UUID.randomUUID()))
    )
  )
}
