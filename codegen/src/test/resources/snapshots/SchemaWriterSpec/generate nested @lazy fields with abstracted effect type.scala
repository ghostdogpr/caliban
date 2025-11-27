import Types._

object Types {

  final case class Foo[F[_]](bar: Bar[F])
  final case class Bar[F[_]](baz: F[Baz[F]])
  final case class Baz[F[_]](x: String, y: F[String])

}

object Operations {

  final case class Query[F[_]](
    foo: F[Foo[F]]
  )

}
