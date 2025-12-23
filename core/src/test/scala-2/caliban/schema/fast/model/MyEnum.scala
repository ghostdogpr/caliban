package caliban.schema.fast.model

import caliban.schema.Annotations.GQLName
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

sealed trait MyEnum

object MyEnum {

  case object SOME_CASE extends MyEnum

  case object ANOTHER_CASE extends MyEnum

  @GQLName("RENAMED_CASE")
  case object WHAT extends MyEnum

  implicit val argBuilder: ArgBuilder[MyEnum] = ArgBuilder.gen[MyEnum]

  implicit val fastArgBuilder: FastArgBuilderWrapper[MyEnum] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[MyEnum])
}
