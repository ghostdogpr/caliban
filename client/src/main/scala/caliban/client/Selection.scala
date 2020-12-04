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
      variables: Map[String, (__Value, String)]
    ): (String, Map[String, (__Value, String)]) = {
      val (newArgs, newVariables) = arguments.foldLeft((List.empty[String], variables)) {
        case ((args, variables), arg) =>
          val (arg2, variables2) = arg.toGraphQL(useVariables, variables)
          (arg2 :: args, variables2)
      }
      (s"@$name(${newArgs.reverse.mkString(",")})", newVariables)
    }
  }
}
