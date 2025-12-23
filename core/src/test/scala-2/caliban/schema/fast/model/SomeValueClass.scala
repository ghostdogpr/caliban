package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

class SomeValueClass(val value: ClassWithOneField) extends AnyVal

object SomeValueClass {
  implicit val argBuilder: ArgBuilder[SomeValueClass] = ArgBuilder.gen[SomeValueClass]

  implicit val fastArgBuilder: FastArgBuilderWrapper[SomeValueClass] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[SomeValueClass])
}
