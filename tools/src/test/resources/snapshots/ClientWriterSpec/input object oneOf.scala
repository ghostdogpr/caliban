import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait CharacterInput {
    protected def encode: __Value
  }

  object CharacterInput {
    final case class Name(name: String)                       extends CharacterInput {
      protected def encode: __Value = __ObjectValue(List("name" -> implicitly[ArgEncoder[String]].encode(name)))
    }
    final case class Nicknames(nicknames: List[String] = Nil) extends CharacterInput {
      protected def encode: __Value = __ObjectValue(
        List("nicknames" -> __ListValue(nicknames.map(value => implicitly[ArgEncoder[String]].encode(value))))
      )
    }

    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value = value.encode
    }
  }

}
