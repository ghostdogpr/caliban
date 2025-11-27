import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH extends Origin { val value: String = "EARTH" }
    case object BELT  extends Origin { val value: String = "BELT"  }

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH => __EnumValue("EARTH")
      case Origin.BELT  => __EnumValue("BELT")
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, BELT)
  }

}
