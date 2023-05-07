package caliban.execution

sealed trait Feature

object Feature {
  case object Defer  extends Feature
  case object Stream extends Feature
}
