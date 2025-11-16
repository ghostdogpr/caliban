import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Origin extends scala.Product with scala.Serializable { def value: String }
  object Origin {
    case object EARTH extends Origin { val value: String = "EARTH" }
    case object MARS  extends Origin { val value: String = "MARS"  }
    case object BELT  extends Origin { val value: String = "BELT"  }

    implicit val decoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin]    = {
      case Origin.EARTH => __EnumValue("EARTH")
      case Origin.MARS  => __EnumValue("MARS")
      case Origin.BELT  => __EnumValue("BELT")
    }

    val values: scala.collection.immutable.Vector[Origin] = scala.collection.immutable.Vector(EARTH, MARS, BELT)
  }

  final case class Routes(origin: Origin, destinations: List[com.example.Destination] = Nil)
  object Routes {
    implicit val encoder: ArgEncoder[Routes] = new ArgEncoder[Routes] {
      override def encode(value: Routes): __Value =
        __ObjectValue(
          List(
            "origin"       -> implicitly[ArgEncoder[Origin]].encode(value.origin),
            "destinations" -> __ListValue(
              value.destinations.map(value => implicitly[ArgEncoder[com.example.Destination]].encode(value))
            )
          )
        )
    }
  }

}
