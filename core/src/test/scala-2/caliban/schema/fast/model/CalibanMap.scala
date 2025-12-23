package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

case class CalibanMap(map: Map[String, String])

object CalibanMap {

  implicit lazy val argBuilder: ArgBuilder[CalibanMap] = ArgBuilder.gen[CalibanMap]

  implicit lazy val fastArgBuilder: FastArgBuilderWrapper[CalibanMap] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[CalibanMap])
}
