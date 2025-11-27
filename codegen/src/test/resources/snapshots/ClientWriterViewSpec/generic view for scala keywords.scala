import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type `package`
  object `package` {

    final case class packageView(name: scala.Option[String])

    type ViewSelection = SelectionBuilder[`package`, packageView]

    def view: ViewSelection = name.map(name => packageView(name))

    def name: SelectionBuilder[`package`, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("name", OptionOf(Scalar()))
  }

  type `match`
  object `match` {

    final case class matchView[PackageSelection](
      `package`: scala.Option[PackageSelection],
      version: scala.Option[String]
    )

    type ViewSelection[PackageSelection] = SelectionBuilder[`match`, matchView[PackageSelection]]

    def view[PackageSelection](
      packageSelection: SelectionBuilder[`package`, PackageSelection]
    ): ViewSelection[PackageSelection] = (`package`(packageSelection) ~ version).map { case (package$, version) =>
      matchView(package$, version)
    }

    def `package`[A](innerSelection: SelectionBuilder[`package`, A]): SelectionBuilder[`match`, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("package", OptionOf(Obj(innerSelection)))
    def version: SelectionBuilder[`match`, scala.Option[String]]                                                 =
      _root_.caliban.client.SelectionBuilder.Field("version", OptionOf(Scalar()))
  }

}
