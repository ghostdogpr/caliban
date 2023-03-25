package caliban.client

import caliban.client.GraphQLResponseError.Location
import com.github.plokhotnyuk.jsoniter_scala.core.{ JsonReader, JsonValueCodec, JsonWriter }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.annotation.switch

/**
 * An GraphQL error as returned by the server.
 * @param message error message
 * @param locations line and column that caused the error in the initial query
 * @param path path of the field that caused the error
 */
case class GraphQLResponseError(
  message: String,
  locations: Option[List[Location]],
  path: Option[List[Either[String, Int]]],
  extensions: Option[__Value]
)

object GraphQLResponseError {

  case class Location(line: Int, column: Int)

  private implicit val eitherCodec: JsonValueCodec[Either[String, Int]] = new JsonValueCodec[Either[String, Int]] {
    override def decodeValue(in: JsonReader, default: Either[String, Int]): Either[String, Int] = {
      val b = in.nextToken()
      in.rollbackToken()
      (b: @switch) match {
        case '"'                                                             => Left(in.readString(null))
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Right(in.readInt())
        case _                                                               => in.decodeError("expected int or string")
      }
    }

    override def encodeValue(x: Either[String, Int], out: JsonWriter): Unit =
      x.fold(out.writeVal, out.writeVal)

    override def nullValue: Either[String, Int] =
      null.asInstanceOf[Either[String, Int]]
  }

  implicit val locationCodec: JsonValueCodec[Location] = JsonCodecMaker.makeCirceLike[Location]

  implicit val jsonCodec: JsonValueCodec[GraphQLResponseError] = JsonCodecMaker.makeCirceLike[GraphQLResponseError]

}
