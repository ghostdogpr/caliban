package caliban.introspection.adt

case class __Type(
  kind: __TypeKind,
  name: Option[String] = None,
  description: Option[String] = None,
  fields: __DeprecatedArgs => Option[List[__Field]] = _ => None,
  interfaces: Option[List[__Type]] = None,
  possibleTypes: Option[List[__Type]] = None,
  enumValues: __DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
  inputFields: Option[List[__InputValue]] = None,
  ofType: Option[__Type] = None
) {
  def |+|(that: __Type): __Type = __Type(
    kind,
    (name ++ that.name).reduceOption((_, b) => b),
    (description ++ that.description).reduceOption((_, b) => b),
    args =>
      (fields(args) ++ that.fields(args)).reduceOption((a, b) => a.filterNot(f => b.exists(_.name == f.name)) ++ b),
    (interfaces ++ that.interfaces).reduceOption((a, b) => a.filterNot(t => b.exists(_.name == t.name)) ++ b),
    (possibleTypes ++ that.possibleTypes).reduceOption((a, b) => a.filterNot(t => b.exists(_.name == t.name)) ++ b),
    args =>
      (enumValues(args) ++ that.enumValues(args))
        .reduceOption((a, b) => a.filterNot(v => b.exists(_.name == v.name)) ++ b),
    (inputFields ++ that.inputFields).reduceOption((a, b) => a.filterNot(t => b.exists(_.name == t.name)) ++ b),
    (ofType ++ that.ofType).reduceOption(_ |+| _)
  )
}
