package models

import caliban.schema.{ ArgBuilder, Schema }
import caliban.Value.StringValue

case class ID(id: String) extends AnyVal

object ID {
  implicit val schema: Schema[Any, ID]    = Schema.scalarSchema[ID]("ID", None, None, None, id => StringValue(id.id))
  implicit val argBuilder: ArgBuilder[ID] = ArgBuilder.string.map(ID(_))
}
