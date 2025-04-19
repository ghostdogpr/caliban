package caliban.federation.v2x

import caliban.InputValue
import caliban.Value.{ BooleanValue, StringValue }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2 {
  case class GQLShareable() extends GQLDirective(Shareable)

  val Shareable = Directive("shareable")

  case class GQLInaccessible() extends GQLDirective(Inaccessible)

  val Inaccessible = Directive("inaccessible")

  case class GQLOverride(from: String) extends GQLDirective(Override(from))

  object Override {
    def apply(from: String): Directive =
      Directive("override", Map("from" -> StringValue(from)))
  }

  case class GQLTag(name: String) extends GQLDirective(Tag(name))

  object Tag {
    def apply(name: String): Directive =
      Directive("tag", Map("name" -> StringValue(name)))
  }

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
