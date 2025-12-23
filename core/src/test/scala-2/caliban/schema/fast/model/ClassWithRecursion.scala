package caliban.schema.fast.model

import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithRecursion(x: Int, rest: Option[ClassWithRecursion])

object ClassWithRecursion {

  implicit lazy val fastArgBuilder: ArgBuilder[ClassWithRecursion] = caliban.schema.ArgBuilder.genFast

  implicit val fastArgBuilderWrapper: FastArgBuilderWrapper[ClassWithRecursion] =
    new FastArgBuilderWrapper(fastArgBuilder)

}
