package caliban.schema.fast.model

import caliban.schema.Annotations.GQLDefault
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

final case class ClassWithDefaultValue(
  x: String,
  @GQLDefault("{ myField: 42 }") defaultObject: ClassWithOneField,
  @GQLDefault("42") defaultInt: Int,
  @GQLDefault("9223372036854775807") defaultLong: Long,
  @GQLDefault("9223372036854775808") defaultBigInt: BigInt,
  @GQLDefault("42.5") defaultFloat: Float,
  @GQLDefault("1e-50") defaultDouble: Double,
  @GQLDefault("42.999999999999999") defaultBigDecimal: BigDecimal,
  @GQLDefault("[42, 43, 44]") defaultList: List[Int],
  @GQLDefault("\"str\"") defaultStr: String,
  @GQLDefault("true") defaultBoolean: Boolean
)

object ClassWithDefaultValue {
  implicit val argBuilder: ArgBuilder[ClassWithDefaultValue] = ArgBuilder.gen[ClassWithDefaultValue]

  implicit val fastArgBuilder: FastArgBuilderWrapper[ClassWithDefaultValue] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[ClassWithDefaultValue])
}
