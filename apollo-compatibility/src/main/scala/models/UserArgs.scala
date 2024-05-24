package models

import caliban.schema.{ ArgBuilder, Schema }

case class UserArgs(email: ID)

object UserArgs {
  implicit val schema: Schema[Any, UserArgs]    = Schema.gen
  implicit val argBuilder: ArgBuilder[UserArgs] = ArgBuilder.gen
}
