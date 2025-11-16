import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH                         extends Origin { val value: String = "EARTH" }
    case object MARS                          extends Origin { val value: String = "MARS"  }
    case object BELT                          extends Origin { val value: String = "BELT"  }
    final case class __Unknown(value: String) extends Origin

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case __StringValue(other)   => Right(Origin.__Unknown(other))
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH            => __EnumValue("EARTH")
      case Origin.MARS             => __EnumValue("MARS")
      case Origin.BELT             => __EnumValue("BELT")
      case Origin.__Unknown(value) => __EnumValue(value)
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS, BELT)
  }

}
