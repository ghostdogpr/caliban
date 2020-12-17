package caliban.execution

/**
 * Defines which type of parallelism to use when executing queries
 */
sealed trait QueryExecution

object QueryExecution {

  /**
   * Run effectful fields sequentially.
   */
  case object Sequential extends QueryExecution

  /**
   * Run effectful fields in parallel (default).
   */
  case object Parallel extends QueryExecution

  /**
   * Run effectful fields sequentially but batch effects wrapped with `ZQuery.fromRequest`.
   * This mode is recommended when most of your effects are relying on ZQuery as it avoids forking unnecessary fibers.
   */
  case object Batched extends QueryExecution
}
