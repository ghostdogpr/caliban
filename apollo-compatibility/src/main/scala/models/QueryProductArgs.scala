package models

import caliban.schema.{ ArgBuilder, Schema }

case class QueryProductArgs(id: ID)

object QueryProductArgs {
  implicit val argBuilder: ArgBuilder[QueryProductArgs] = ArgBuilder.gen
  implicit val schema: Schema[Any, QueryProductArgs]    = Schema.gen
}
