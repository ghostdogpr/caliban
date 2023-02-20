package caliban.federation

import caliban.federation.FederationHelpers._FieldSet
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }

class FederationV1
    extends FederationSupport(
      List(
        __Directive(
          "external",
          Some("The @external directive is used to mark a field as owned by another service"),
          locations = Set(__DirectiveLocation.FIELD_DEFINITION),
          args = Nil,
          repeatable = false
        ),
        __Directive(
          "requires",
          None,
          locations = Set(__DirectiveLocation.FIELD_DEFINITION),
          args = _FieldSet :: Nil,
          repeatable = false
        ),
        __Directive(
          "provides",
          None,
          locations = Set(__DirectiveLocation.FIELD_DEFINITION),
          args = _FieldSet :: Nil,
          repeatable = false
        ),
        __Directive(
          "key",
          None,
          locations = Set(__DirectiveLocation.OBJECT, __DirectiveLocation.INTERFACE),
          args = _FieldSet :: Nil,
          repeatable = true
        ),
        __Directive(
          "extends",
          None,
          locations = Set(__DirectiveLocation.OBJECT, __DirectiveLocation.INTERFACE),
          Nil,
          repeatable = false
        )
      ),
      Nil
    )
