package caliban.execution

import caliban.PathValue
import caliban.schema.ReducedStep

sealed trait Deferred[-R] {
  def path: List[PathValue]
  def label: Option[String]
}

case class DeferredFragment[-R](
  path: List[PathValue],
  step: ReducedStep[R],
  label: Option[String]
) extends Deferred[R]

case class DeferredStream[-R](
  path: List[PathValue],
  step: ReducedStep.StreamStep[R],
  label: Option[String],
  startFrom: Int
) extends Deferred[R]
