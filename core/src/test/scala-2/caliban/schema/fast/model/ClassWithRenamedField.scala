package caliban.schema.fast.model

import caliban.schema.Annotations.GQLName
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithRenamedField(@GQLName("renamed") myField: Int, anotherField: Int)

object ClassWithRenamedField {
  implicit val argBuilder: ArgBuilder[ClassWithRenamedField] = ArgBuilder.gen[ClassWithRenamedField]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithRenamedField] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithRenamedField])
}
