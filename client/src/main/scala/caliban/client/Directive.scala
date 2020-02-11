package caliban.client

case class Directive(name: String, arguments: List[Argument[_]] = Nil) {
  def toGraphQL: String = {
    val args = arguments.map(_.toGraphQL).mkString(",")
    s"@$name($args)"
  }
}
