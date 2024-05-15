package models

import caliban.schema.Schema

@GQLKey("email")
@GQLExtend
case class User(
  @GQLExternal email: ID,
  @GQLExternal totalProductsCreated: Option[Int],
  @GQLOverride("users") name: Option[String],
  @GQLRequires("totalProductsCreated yearsOfEmployment") averageProductsCreatedPerYear: Option[Int],
  @GQLExternal yearsOfEmployment: Int
)

object User {
  implicit val schema: Schema[Any, User] = Schema.gen
}
