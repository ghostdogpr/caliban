package caliban.execution

import caliban.Value.{ BooleanValue, StringValue }
import caliban.parsing.adt.Directive

case class Fragment(name: Option[String], directives: List[Directive]) {}

object Fragment {
  object IsDeferred {
    def unapply(fragment: Fragment): Option[Option[String]] =
      fragment.directives.collectFirst {
        case Directive("defer", args, _) if args.get("if").forall {
              case BooleanValue(v) => v
              case _               => true
            } =>
          args.get("name").collect { case StringValue(v) => v }
      }
  }
}
