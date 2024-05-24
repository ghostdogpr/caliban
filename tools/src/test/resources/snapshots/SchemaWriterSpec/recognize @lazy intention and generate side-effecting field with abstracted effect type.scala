object Types {

  final case class Foo[F[_]](bar: String, baz: F[String])

}
