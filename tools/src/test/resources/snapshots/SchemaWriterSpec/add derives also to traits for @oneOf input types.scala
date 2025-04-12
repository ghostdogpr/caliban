import caliban.schema.Annotations._

object Types {

  @GQLOneOfInput
  sealed trait FooInput extends scala.Product with scala.Serializable
      derives caliban.schema.Schema.SemiAuto,
        caliban.schema.ArgBuilder
  object FooInput {
    final case class OptionA(optionA: String) extends FooInput
        derives caliban.schema.Schema.SemiAuto,
          caliban.schema.ArgBuilder
    final case class OptionB(optionB: Int)    extends FooInput
        derives caliban.schema.Schema.SemiAuto,
          caliban.schema.ArgBuilder
  }

}
