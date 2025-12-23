package caliban.schema.fast.model

import caliban.schema.Annotations.GQLOneOfInput
import caliban.schema.ArgBuilder
import caliban.schema.fast.FastArgBuilderWrapper

@GQLOneOfInput
sealed trait SomeOneOf

object SomeOneOf {
  case class ById(id: Int) extends SomeOneOf

  object ById {
    implicit val argBuilder: ArgBuilder[ById] = ArgBuilder.genFast[ById]
  }

  case class ByClassWithOneField(field: ClassWithOneField) extends SomeOneOf

  object ByClassWithOneField {
    implicit val argBuilder: ArgBuilder[ByClassWithOneField] = ArgBuilder.genFast[ByClassWithOneField]
  }

  implicit val argBuilder: ArgBuilder[SomeOneOf] = ArgBuilder.gen[SomeOneOf]

  implicit val fastArgBuilder: FastArgBuilderWrapper[SomeOneOf] =
    new FastArgBuilderWrapper(ArgBuilder.genFast[SomeOneOf])
}
