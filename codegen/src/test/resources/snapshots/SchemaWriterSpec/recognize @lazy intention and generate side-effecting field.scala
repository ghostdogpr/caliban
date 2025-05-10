package snapshots.SchemaWriterSpec

object Types {

  final case class Foo(bar: String, baz: zio.UIO[String])

}
