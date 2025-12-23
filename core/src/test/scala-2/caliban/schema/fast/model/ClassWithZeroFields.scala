package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithZeroFields()

object ClassWithZeroFields {
  implicit val argBuilder: ArgBuilder[ClassWithZeroFields] = ArgBuilder.gen[ClassWithZeroFields]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithZeroFields] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithZeroFields])
}
