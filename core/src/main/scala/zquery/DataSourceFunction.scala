package zquery

/**
 * A `DataSourceFunction[R, R1]` is a universally quantified function from
 * values of type `DataSource[R, A]` to values of type `DataSource[R1, A]` for
 * all types `A`. This is used internally by the library to describe functions
 * for transforming data sources that do not change the type of requests that a
 * data source is able to execute.
 */
trait DataSourceFunction[+R, -R1] { self =>

  def apply[A](dataSource: DataSource.Service[R, A]): DataSource.Service[R1, A]

  /**
   * A symbolic alias for `compose`.
   */
  final def <<<[R0](that: DataSourceFunction[R0, R]): DataSourceFunction[R0, R1] =
    self compose that

  /**
   * A symbolic alias for `andThen`.
   */
  final def >>>[R2](that: DataSourceFunction[R1, R2]): DataSourceFunction[R, R2] =
    self andThen that

  /**
   * Creates a new data source function by applying this data source function
   * followed by that data source function.
   */
  final def andThen[R2](that: DataSourceFunction[R1, R2]): DataSourceFunction[R, R2] =
    new DataSourceFunction[R, R2] {
      def apply[A](dataSource: DataSource.Service[R, A]): DataSource.Service[R2, A] =
        that(self(dataSource))
    }

  /**
   * Creates a new data source function by applying that data source function
   * followed by this data source function.
   */
  final def compose[R0](that: DataSourceFunction[R0, R]): DataSourceFunction[R0, R1] =
    new DataSourceFunction[R0, R1] {
      def apply[A](dataSource: DataSource.Service[R0, A]): DataSource.Service[R1, A] =
        self(that(dataSource))
    }
}

object DataSourceFunction {

  /**
   * A data source function that provides a data source with its required
   * environment.
   */
  def provide[R](r: Described[R]): DataSourceFunction[R, Any] =
    provideSome(Described(_ => r.value, s"_ => ${r.description}"))

  /**
   * A data source function that provides a data sources with part of its
   * required environment.
   */
  def provideSome[R, R1](f: Described[R1 => R]): DataSourceFunction[R, R1] =
    new DataSourceFunction[R, R1] {
      def apply[A](dataSource: DataSource.Service[R, A]): DataSource.Service[R1, A] =
        dataSource.provideSome(f)
    }
}
