import caliban.client._
import caliban.client.__Value._

object Client {

  final case class CharacterInput(wait$ : String)
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(List("wait" -> implicitly[ArgEncoder[String]].encode(value.wait$)))
    }
  }

}
