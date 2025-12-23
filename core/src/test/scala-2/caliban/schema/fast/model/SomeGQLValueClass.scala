package caliban.schema.fast.model

import caliban.schema.Annotations.GQLValueType
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

@GQLValueType
final case class SomeGQLValueClass(value: ClassWithOneField)

object SomeGQLValueClass {
  implicit val argBuilder: ArgBuilder[SomeGQLValueClass] = ArgBuilder.gen[SomeGQLValueClass]

  implicit val fastArgBuilder: FastArgBuilderWrapper[SomeGQLValueClass] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[SomeGQLValueClass])
}
