package caliban.introspection.adt

import caliban.parsing.adt.Directive

case class __Type(
  kind: __TypeKind,
  name: Option[String] = None,
  description: Option[String] = None,
  fields: __DeprecatedArgs => Option[List[__Field]] = _ => None,
  interfaces: () => Option[List[__Type]] = () => None,
  possibleTypes: Option[List[__Type]] = None,
  enumValues: __DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
  inputFields: Option[List[__InputValue]] = None,
  ofType: Option[__Type] = None,
  directives: Option[List[Directive]] = None,
  origin: Option[String] = None
) {
  def |+|(that: __Type): __Type = __Type(
    kind,
    (name ++ that.name).reduceOption((_, b) => b),
    (description ++ that.description).reduceOption((_, b) => b),
    args => (fields(args) ++ that.fields(args)).reduceOption(_ ++ _),
    () => (interfaces() ++ that.interfaces()).reduceOption(_ ++ _),
    (possibleTypes ++ that.possibleTypes).reduceOption(_ ++ _),
    args => (enumValues(args) ++ that.enumValues(args)).reduceOption(_ ++ _),
    (inputFields ++ that.inputFields).reduceOption(_ ++ _),
    (ofType ++ that.ofType).reduceOption(_ |+| _),
    (directives ++ that.directives).reduceOption(_ ++ _),
    (origin ++ that.origin).reduceOption((_, b) => b)
  )
}
