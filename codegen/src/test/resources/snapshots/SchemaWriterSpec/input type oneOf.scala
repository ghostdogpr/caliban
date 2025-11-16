import caliban.schema.Annotations._

object Types {

  final case class Character(name: String)

  @GQLOneOfInput
  sealed trait CharacterArgs extends scala.Product with scala.Serializable
  object CharacterArgs {
    final case class Foo(foo: String) extends CharacterArgs
    final case class Bar(bar: Int)    extends CharacterArgs
  }

}
