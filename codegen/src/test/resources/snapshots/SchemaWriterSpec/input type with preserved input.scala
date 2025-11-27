import caliban.schema.Annotations._

object Types {

  final case class Character(name: String)
  @GQLInputName("CharacterInput")
  final case class CharacterInput(name: String)

}
