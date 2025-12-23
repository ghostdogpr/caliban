package caliban.schema.fast.model

import caliban.schema.Annotations.GQLValueType
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

@GQLValueType
final case class CalibanValueType(value: Option[String])

object CalibanValueType {
  implicit val argBuilder: ArgBuilder[CalibanValueType] = ArgBuilder.gen[CalibanValueType]

  implicit val fastArgBuilder: FastArgBuilderWrapper[CalibanValueType] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[CalibanValueType])
}
