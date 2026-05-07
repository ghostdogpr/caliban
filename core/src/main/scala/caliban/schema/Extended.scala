package caliban.schema

import caliban.ResponseValue.ObjectValue

case class Extended[A](value: A, extensions: ObjectValue)
