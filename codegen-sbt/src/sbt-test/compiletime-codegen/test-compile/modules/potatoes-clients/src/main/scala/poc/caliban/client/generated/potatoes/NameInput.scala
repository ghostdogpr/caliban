package poc.caliban.client.generated.potatoes

import caliban.client._
import caliban.client.__Value._

final case class NameInput(value: String)
object NameInput {
  implicit val encoder: ArgEncoder[NameInput] = new ArgEncoder[NameInput] {
    override def encode(value: NameInput): __Value =
      __ObjectValue(
        List("value" -> implicitly[ArgEncoder[String]].encode(value.value))
      )
  }
}

