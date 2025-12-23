package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithOneField(myField: Long)

object ClassWithOneField {
  implicit val argBuilder: ArgBuilder[ClassWithOneField] = ArgBuilder.gen[ClassWithOneField]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithOneField] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithOneField])
}
