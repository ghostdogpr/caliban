package caliban.client

sealed trait Selection

object Selection {
  case class InlineFragment(onType: String, selectionSet: List[Selection]) extends Selection
  case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument[_]],
    directives: List[Directive],
    selectionSet: List[Selection],
    code: Int
  ) extends Selection

  case class Directive(name: String, arguments: List[Argument[_]] = Nil) {
    def toGraphQL(
      useVariables: Boolean,
      variables: Map[String, (Value, String)]
    ): (String, Map[String, (Value, String)]) = {
      val (args, v) = arguments.foldRight((List.empty[String], variables)) {
        case (arg, (args, v2)) =>
          val (a2, v3) = arg.toGraphQL(useVariables, v2)
          (a2 :: args, v3)
      }
      (s"@$name(${args.mkString(",")})", v)
    }
  }
}
