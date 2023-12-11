package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Annotations.GQLValueType
import caliban.schema.DerivationUtils.*
import caliban.schema.Types.makeScalar
import magnolia1.TypeInfo

import scala.annotation.threadUnsafe

final private class ValueTypeSchema[R, A](
  _schema: => Schema[R, Any],
  info: TypeInfo,
  anns: List[Any]
) extends Schema[R, A] {
  private val name = getName(anns, info)

  @threadUnsafe
  private lazy val schema = _schema

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _ = schema
    if (anns.contains(GQLValueType(true))) makeScalar(name, getDescription(anns))
    else schema.toType_(isInput, isSubscription)
  }

  def resolve(value: A): Step[R] = schema.resolve(value.asInstanceOf[Product].productElement(0))
}
