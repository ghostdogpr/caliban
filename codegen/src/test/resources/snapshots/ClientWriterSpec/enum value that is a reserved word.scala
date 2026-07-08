import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait E extends scala.Product with scala.Serializable { def value: String }
  object E {
    case object `type`  extends E { val value: String = "type"  }
    case object `match` extends E { val value: String = "match" }

    implicit val decoder: ScalarDecoder[E] = {
      case __StringValue("type")  => Right(E.`type`)
      case __StringValue("match") => Right(E.`match`)
      case other                  => Left(DecodingError(s"Can't build E from input $other"))
    }
    implicit val encoder: ArgEncoder[E]    = {
      case E.`type`  => __EnumValue("type")
      case E.`match` => __EnumValue("match")
    }

    val values: scala.collection.immutable.Vector[E] = scala.collection.immutable.Vector(`type`, `match`)
  }

}
