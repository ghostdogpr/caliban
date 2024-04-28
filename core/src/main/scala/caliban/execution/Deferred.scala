package caliban.execution

import caliban.PathValue
import caliban.schema.ReducedStep

case class Deferred[-R](
  path: List[PathValue],
  step: ReducedStep[R],
  label: Option[String]
)
