package caliban.federation.v2x

import caliban.InputValue
import caliban.InputValue.ListValue
import caliban.Value.StringValue
import caliban.parsing.adt.Directive

trait Extension {
  def toDirective: Directive
}

case class ComposeDirective(name: String) extends Extension {
  def toDirective: Directive = Directive("composeDirective", Map("name" -> StringValue(name)))
}

case class Link(url: String, `import`: List[Import]) extends Extension {
  def toDirective: Directive =
    Directive("link", Map("url" -> StringValue(url), "import" -> ListValue(`import`.map(_.toInputValue))))
}

case class Import(name: String, as: Option[String] = None) {
  def toInputValue: InputValue = as.fold[InputValue](StringValue(name))(alias =>
    InputValue.ObjectValue(
      Map("name" -> StringValue(name), "as" -> StringValue(alias))
    )
  )
}
