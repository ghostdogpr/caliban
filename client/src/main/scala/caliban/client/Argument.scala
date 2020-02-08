package caliban.client

case class Argument[A](name: String, value: A)(implicit encoder: ArgEncoder[A]) {
  def toGraphQL: String = {
    val v = encoder.encode(value)
    if (v.nonEmpty) s"$name: $v" else v
  }
}
