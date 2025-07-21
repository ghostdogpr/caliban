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

  case class FragmentSpread(
    name: Option[String],
    on: String,
    selectionSet: List[Selection],
    directives: List[Directive]
  ) extends Selection {
    val code: Int = hashCode()

    def toGraphQL(
      useVariables: Boolean,
      dropNullInputValues: Boolean,
      variables: Map[String, (__Value, String)]
    ): (List[String], Map[String, (__Value, String)]) = {
      val (query, inner, variables2) = SelectionBuilder.toGraphQL(
        selectionSet,
        useVariables,
        dropNullInputValues,
        variables
      )

      (
        s"fragment ${name.getOrElse("f" + math.abs(code))} on $on{$query}" :: inner,
        variables2
      )
    }
  }

  case class Directive(name: String, arguments: List[Argument[_]] = Nil) {
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
