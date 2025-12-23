package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithMultipleFields(id: Long, age: Int, name: Option[String], kek: Option[ClassWithOneField])

object ClassWithMultipleFields {
  implicit val argBuilder: ArgBuilder[ClassWithMultipleFields] = ArgBuilder.gen[ClassWithMultipleFields]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithMultipleFields] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithMultipleFields])
}
