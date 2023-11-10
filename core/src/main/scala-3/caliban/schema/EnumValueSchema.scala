package caliban.schema

import caliban.Value.EnumValue
import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import magnolia1.TypeInfo

final private class EnumValueSchema[R, A](
  info: TypeInfo,
  anns: List[Any]
) extends Schema[R, A] {

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (isInput) mkInputObject[R](anns, Nil, info)(isInput, isSubscription)
    else mkObject[R](anns, Nil, info)(isInput, isSubscription)

  private val step               = PureStep(EnumValue(getName(anns, info)))
  def resolve(value: A): Step[R] = step
}
