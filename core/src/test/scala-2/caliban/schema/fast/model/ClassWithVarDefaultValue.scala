package caliban.schema.fast.model

import caliban.schema.Annotations.GQLDefault
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithVarDefaultValue(x: String, @GQLDefault("$y") defaultValue: Int)

object ClassWithVarDefaultValue {
  implicit val argBuilder: ArgBuilder[ClassWithVarDefaultValue] = ArgBuilder.gen[ClassWithVarDefaultValue]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithVarDefaultValue] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithVarDefaultValue])
}
