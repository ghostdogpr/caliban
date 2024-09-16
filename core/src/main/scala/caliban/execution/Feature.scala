package caliban.execution

import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue }
import caliban.parsing.adt.Directives
import caliban.schema.Types

sealed trait Feature {
  def index: Int
  def mask: Int
  def directives: List[__Directive]
}

object Feature {
  type Flags = Int

  private def isEnabled(flags: Flags, mask: Int): Boolean = (flags & mask) != 0
  def isDeferEnabled(flags: Flags): Boolean               = isEnabled(flags, Defer.mask)
  def isStreamEnabled(flags: Flags): Boolean              = isEnabled(flags, Stream.mask)

  case object Defer extends Feature {
    final val index: Int = 0
    final val mask: Int  = 1 << index

    final val directives: List[__Directive] = List(
      __Directive(
        Directives.Defer,
        Some(
          "Marks a fragment as being optionally deferrable. Allowing the backend to split the query and return non-deferred parts first. This implicitly uses a streaming transport protocol which requires client support."
        ),
        Set(__DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
        _ =>
          List(
            __InputValue("if", None, () => Types.boolean, None),
            __InputValue("label", None, () => Types.string, None)
          ),
        isRepeatable = false
      )
    )
  }

  case object Stream extends Feature {
    final val index: Int = 1
    final val mask: Int  = 1 << index

    final val directives: List[__Directive] = List(
      __Directive(
        Directives.Stream,
        Some(
          "Marks a field as being optionally streamable. Allowing the backend to split the returned list value into separate payloads, only returning the first part of the list immediately. This implicitly uses a streaming transport protocol which requires client support."
        ),
        Set(__DirectiveLocation.FIELD),
        _ =>
          List(
            __InputValue("if", None, () => Types.boolean, None),
            __InputValue("initialCount", None, () => Types.int, None),
            __InputValue("label", None, () => Types.string, None)
          ),
        isRepeatable = false
      )
    )
  }
}
