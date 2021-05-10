package caliban.schema.macros

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]) {
  def full: String = s"$owner.$short"
}
