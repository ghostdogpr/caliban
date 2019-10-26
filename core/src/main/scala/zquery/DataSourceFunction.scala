package zquery

/**
 * A `DataSourceFunction[R, E, R1, E1]` is a universally quantified function
 * from values of type `DataSource[R, E, A]` to values of type
 * `DataSource[R1, E1, A]` for all types `A`. This is used internally by the
 * library to describe functions for transforming data sources that do not
 * change the type of requests that a data source is able to execute.
 */
trait DataSourceFunction[+R, -E, -R1, +E1] { self =>

  def apply[A](dataSource: DataSource.Service[R, E, A]): DataSource.Service[R1, E1, A]

  /**
   * A symbolic alias for `compose`.
   */
  final def <<<[R0, E0](that: DataSourceFunction[R0, E0, R, E]): DataSourceFunction[R0, E0, R1, E1] =
    self compose that

  /**
   * A symbolic alias for `andThen`.
   */
  final def >>>[R2, E2](that: DataSourceFunction[R1, E1, R2, E2]): DataSourceFunction[R, E, R2, E2] =
    self andThen that

  /**
   * Creates a new data source function by applying this data source function
   * followed by that data source function.
   */
  final def andThen[R2, E2](that: DataSourceFunction[R1, E1, R2, E2]): DataSourceFunction[R, E, R2, E2] =
    new DataSourceFunction[R, E, R2, E2] {
      def apply[A](dataSource: DataSource.Service[R, E, A]): DataSource.Service[R2, E2, A] =
        that(self(dataSource))
    }

  /**
   * Creates a new data source function by applying that data source function
   * followed by this data source function.
   */
  final def compose[R0, E0](that: DataSourceFunction[R0, E0, R, E]): DataSourceFunction[R0, E0, R1, E1] =
    new DataSourceFunction[R0, E0, R1, E1] {
      def apply[A](dataSource: DataSource.Service[R0, E0, A]): DataSource.Service[R1, E1, A] =
        self(that(dataSource))
    }
}

object DataSourceFunction {

  /**
   * A data source function that maps failures produced by a data source using
   * the specified function.
   */
  final def mapError[R, E, E1](name: String)(f: E => E1): DataSourceFunction[R, E, R, E1] =
    new DataSourceFunction[R, E, R, E1] {
      def apply[A](dataSource: DataSource.Service[R, E, A]): DataSource.Service[R, E1, A] =
        dataSource.mapError(name)(f)
    }

  /**
   * A data source function that provides a data source with its required
   * environment.
   */
  final def provide[R, E](name: String)(r: R): DataSourceFunction[R, E, Any, E] =
    provideSome(s"_ => $name")(_ => r)

  /**
   * A data source function that provides a data sources with part of its
   * required environment.
   */
  final def provideSome[R, R1, E](name: String)(f: R1 => R): DataSourceFunction[R, E, R1, E] =
    new DataSourceFunction[R, E, R1, E] {
      def apply[A](dataSource: DataSource.Service[R, E, A]): DataSource.Service[R1, E, A] =
        dataSource.provideSome(name)(f)
    }
}
