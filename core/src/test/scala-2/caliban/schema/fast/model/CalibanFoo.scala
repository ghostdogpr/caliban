package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

case class CalibanFoo(value: Option[String])

object CalibanFoo {
  implicit val argBuilder: ArgBuilder[CalibanFoo] = ArgBuilder.gen[CalibanFoo]

  implicit val fastArgBuilder: FastArgBuilderWrapper[CalibanFoo] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[CalibanFoo])
}
