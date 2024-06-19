package caliban.execution

import caliban.Value.{ BooleanValue, IntValue, StringValue }
import caliban.parsing.adt.{ Directive, Directives }

case class Fragment(name: Option[String], directives: List[Directive]) {}

object Fragment {
  object IsDeferred {
    def unapply(fragment: Fragment): Option[Option[String]] =
      fragment.directives.collectFirst {
        case Directive(Directives.Defer, args, _, _) if args.get("if").forall {
              case BooleanValue(v) => v
              case _               => true
            } =>
          args.get("label").collect { case StringValue(v) => v }
      }
  }
}

object IsStream {
  def unapply(field: Field): Option[(Option[String], Option[Int])] =
    field.directives.collectFirst { case Directive(Directives.Stream, args, _, _) =>
      (
        args.get("label").collect { case StringValue(v) => v },
        args.get("initialCount").collect { case v: IntValue => v.toInt }
      )
    }
}
