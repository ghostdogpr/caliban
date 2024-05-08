package caliban.execution

/**
 * Defines which type of parallelism to use when executing queries
 */
sealed trait QueryExecution {
  def tag: Int
}

object QueryExecution {

  /**
   * Run effectful fields sequentially.
   */
  case object Sequential extends QueryExecution {
    final val tag = 0
  }

  /**
   * Run effectful fields in parallel (default).
   */
  case object Parallel extends QueryExecution {
    final val tag = 1
  }

  /**
   * Run effectful fields sequentially but batch effects wrapped with `ZQuery.fromRequest`.
   * This mode is recommended when most of your effects are backed by ZQuery DataSources as it avoids forking unnecessary fibers.
   */
  case object Batched extends QueryExecution {
    final val tag = 2
  }

  /**
   * Run effectful top-level fields in [[Parallel]] mode and nested fields in [[Batched]] mode.
   * This mode is recommended when most of your effects are backed by ZQuery DataSources but want to guarantee that top-level fields are always executed in parallel.
   */
  case object Mixed extends QueryExecution {
    final val tag = 3
  }
}
