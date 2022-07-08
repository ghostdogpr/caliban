package poc.caliban.client.generated.potatoes

import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

sealed trait Color extends scala.Product with scala.Serializable {
  def value: String
}
object Color {
  case object Purple extends Color { val value: String = "Purple" }
  case object Red extends Color { val value: String = "Red" }
  case object White extends Color { val value: String = "White" }
  case object Yellow extends Color { val value: String = "Yellow" }

  implicit val decoder: ScalarDecoder[Color] = {
    case __StringValue("Purple") => Right(Color.Purple)
    case __StringValue("Red")    => Right(Color.Red)
    case __StringValue("White")  => Right(Color.White)
    case __StringValue("Yellow") => Right(Color.Yellow)
    case other                   => Left(DecodingError(s"Can't build Color from input $other"))
  }
  implicit val encoder: ArgEncoder[Color] = {
    case Color.Purple => __EnumValue("Purple")
    case Color.Red    => __EnumValue("Red")
    case Color.White  => __EnumValue("White")
    case Color.Yellow => __EnumValue("Yellow")
  }

  val values: scala.collection.immutable.Vector[Color] =
    scala.collection.immutable.Vector(Purple, Red, White, Yellow)
}

