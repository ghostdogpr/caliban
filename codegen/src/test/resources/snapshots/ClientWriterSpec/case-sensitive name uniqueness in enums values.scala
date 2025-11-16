import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Episode extends scala.Product with scala.Serializable { def value: String }
  object Episode {
    case object NEWHOPE extends Episode { val value: String = "NEWHOPE" }
    case object EMPIRE  extends Episode { val value: String = "EMPIRE"  }
    case object JEDI    extends Episode { val value: String = "JEDI"    }
    case object jedi_1  extends Episode { val value: String = "jedi_1"  }

    implicit val decoder: ScalarDecoder[Episode] = {
      case __StringValue("NEWHOPE") => Right(Episode.NEWHOPE)
      case __StringValue("EMPIRE")  => Right(Episode.EMPIRE)
      case __StringValue("JEDI")    => Right(Episode.JEDI)
      case __StringValue("jedi")    => Right(Episode.jedi_1)
      case other                    => Left(DecodingError(s"Can't build Episode from input $other"))
    }
    implicit val encoder: ArgEncoder[Episode]    = {
      case Episode.NEWHOPE => __EnumValue("NEWHOPE")
      case Episode.EMPIRE  => __EnumValue("EMPIRE")
      case Episode.JEDI    => __EnumValue("JEDI")
      case Episode.jedi_1  => __EnumValue("jedi")
    }

    val values: scala.collection.immutable.Vector[Episode] =
      scala.collection.immutable.Vector(NEWHOPE, EMPIRE, JEDI, jedi_1)
  }

}
