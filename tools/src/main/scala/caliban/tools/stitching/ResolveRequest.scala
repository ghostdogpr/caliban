package caliban.tools.stitching

import caliban.execution.Field

case class ResolveRequest[A](
  args: A,
  field: Field
)
