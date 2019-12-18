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
    (name ++ that.name).reduceOption((a, b) => s"${a}With$b"),
    (description ++ that.description).reduceOption((a, b) => s"$a\n$b"),
    args =>
      (fields(args) ++ that.fields(args)).reduceOption((a, b) => a ++ b.filterNot(f => a.exists(_.name == f.name))),
    (interfaces ++ that.interfaces).reduceOption((a, b) => a ++ b.filterNot(t => a.exists(_.name == t.name))),
    (possibleTypes ++ that.possibleTypes).reduceOption((a, b) => a ++ b.filterNot(t => a.exists(_.name == t.name))),
    args =>
      (enumValues(args) ++ that.enumValues(args))
        .reduceOption((a, b) => a ++ b.filterNot(v => a.exists(_.name == v.name))),
    (inputFields ++ that.inputFields).reduceOption((a, b) => a ++ b.filterNot(t => a.exists(_.name == t.name))),
    (ofType ++ that.ofType).reduceOption(_ |+| _)
  )
}
