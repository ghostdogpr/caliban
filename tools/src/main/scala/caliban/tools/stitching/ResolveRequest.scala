package caliban.tools.stitching

import caliban.execution.Field

final case class ResolveRequest[A](
  args: A,
  field: Field
)
