import caliban.client._
import caliban.client.__Value._

object Client {

  final case class CharacterInput(
    name: String,
    @deprecated
    nickname: scala.Option[String] = None,
    @deprecated("no longer used")
    address: scala.Option[String] = None
  )
  object CharacterInput {
    implicit val encoder: ArgEncoder[CharacterInput] = new ArgEncoder[CharacterInput] {
      override def encode(value: CharacterInput): __Value =
        __ObjectValue(
          List(
            "name"     -> implicitly[ArgEncoder[String]].encode(value.name),
            "nickname" -> value.nickname.fold(__NullValue: __Value)(value =>
              implicitly[ArgEncoder[String]].encode(value)
            ),
            "address"  -> value.address.fold(__NullValue: __Value)(value => implicitly[ArgEncoder[String]].encode(value))
          )
        )
    }
  }

}
