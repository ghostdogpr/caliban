package caliban.execution

import caliban.schema.ReducedStep

case class Deferred[-R](
  path: List[Either[String, Int]],
  step: ReducedStep[R],
  label: Option[String]
)
