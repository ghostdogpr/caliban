package caliban.federation

import caliban.InputValue
import caliban.Value.{ BooleanValue, StringValue }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectives {
  case class GQLProvides(fields: String) extends GQLDirective(Provides(fields))

  object Provides {
    def apply(fields: String): Directive =
      Directive("provides", Map("fields" -> StringValue(fields)))
  }

  case class GQLRequires(fields: String) extends GQLDirective(Requires(fields))

  object Requires {
    def apply(fields: String): Directive =
      Directive("requires", Map("fields" -> StringValue(fields)))
  }

  case class GQLExtend() extends GQLDirective(Extend)

  val Extend = Directive("extends")

  case class GQLExternal() extends GQLDirective(External)

  val External = Directive("external")

  case class GQLKey(fields: String, resolvable: Boolean = true) extends GQLDirective(Key(fields, resolvable))

  object Key {
    def apply(fields: String, resolvable: Boolean = true, name: String = "key"): Directive = {
      val args: List[(String, InputValue)] = List(
        Some("fields" -> StringValue(fields)),
        (if (resolvable) None else Some("resolvable" -> BooleanValue(resolvable)))
      ).flatten

      Directive(name, args.toMap)
    }
  }
}
