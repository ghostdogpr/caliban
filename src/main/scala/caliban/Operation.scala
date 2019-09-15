package caliban

import caliban.schema.Schema

case class Operation[T](schema: Schema[T], resolver: T)
