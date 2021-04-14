package caliban

import caliban.schema.Annotations.GQLDescription

object RenderingSpecSchema {

  case class UserTest(name: String, @GQLDescription("field-description") age: Int)
  case class UserComplex(id: Int, user: UserTest)

  case class QueryTest(allUsers: () => List[UserTest])
  case class MutationTest(id: UserComplex => Boolean)

  val resolverSchema = RootResolver(
    QueryTest(() => List()),
    MutationTest(c => true)
  )
}
