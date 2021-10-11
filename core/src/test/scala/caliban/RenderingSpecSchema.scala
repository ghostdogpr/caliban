package caliban

import caliban.schema.Annotations.GQLDescription

object RenderingSpecSchema {

  final case class UserTest(name: String, @GQLDescription("field-description") age: Int)
  final case class UserComplex(id: Int, user: UserTest)

  final case class UserParams(nameLike: String, @GQLDescription("is user active currently") active: Boolean)

  final case class QueryTest(allUsers: () => List[UserTest])
  final case class MutationTest(
    id: UserComplex => Boolean,
    fetch: UserParams => Boolean
  )

  val resolverSchema: RootResolver[QueryTest,MutationTest,Unit] = RootResolver(
    QueryTest(() => List()),
    MutationTest(c => true, params => true)
  )
}
