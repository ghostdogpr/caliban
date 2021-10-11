package caliban.client

sealed trait Selection

object Selection {
  final case class InlineFragment(onType: String, selectionSet: List[Selection]) extends Selection

  final case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument[_]],
    directives: List[Directive],
    selectionSet: List[Selection],
    code: Int
  ) extends Selection

  final case class Directive(name: String, arguments: List[Argument[_]] = Nil) {
    def toGraphQL(
      useVariables: Boolean,
      dropNullInputValues: Boolean,
      variables: Map[String, (__Value, String)]
    ): (String, Map[String, (__Value, String)]) = {
      val (newArgs, newVariables) = arguments.foldLeft((List.empty[String], variables)) {
        case ((args, variables), arg) =>
          val (arg2, variables2) = arg.toGraphQL(useVariables, dropNullInputValues, variables)
          (arg2 :: args, variables2)
      }
      (s"@$name(${newArgs.reverse.mkString(",")})", newVariables)
    }
  }
}
