package caliban.client

import java.time._
import scala.util.Try
import caliban.client.ScalarDecoder
import caliban.client.ArgEncoder
import caliban.client.__Value._
import caliban.client.CalibanClientError.DecodingError

object CalibanClientTimeSupport {

  // Mirror the server-side TemporalDecoder logic for ScalarDecoder, using idiomatic Try-to-Either conversion
  private def temporalDecoder[A](name: String)(parse: String => A): ScalarDecoder[A] = {
    case __StringValue(value) =>
      Try(parse(value)).toEither.left.map { e =>
        val message = e.getMessage
        if (message == null) DecodingError(s"Can't build $name from input $value")
        else DecodingError(s"Can't build $name from input $value ($message)", Some(e))
      }
    case other => Left(DecodingError(s"Can't build $name from input $other"))
  }

  // Mirror the server-side temporal encoders
  private def temporalEncoder[A](format: A => String): ArgEncoder[A] = value => __StringValue(format(value))

  // Provide the same decoders as the server side
  implicit val instantDecoder: ScalarDecoder[Instant] = temporalDecoder("Instant")(Instant.parse)
  implicit val localDateDecoder: ScalarDecoder[LocalDate] = temporalDecoder("LocalDate")(LocalDate.parse)
  implicit val localTimeDecoder: ScalarDecoder[LocalTime] = temporalDecoder("LocalTime")(LocalTime.parse)
  implicit val localDateTimeDecoder: ScalarDecoder[LocalDateTime] = temporalDecoder("LocalDateTime")(LocalDateTime.parse)
  implicit val offsetTimeDecoder: ScalarDecoder[OffsetTime] = temporalDecoder("OffsetTime")(OffsetTime.parse)
  implicit val offsetDateTimeDecoder: ScalarDecoder[OffsetDateTime] = temporalDecoder("OffsetDateTime")(OffsetDateTime.parse)
  implicit val zonedDateTimeDecoder: ScalarDecoder[ZonedDateTime] = temporalDecoder("ZonedDateTime")(ZonedDateTime.parse)

  // Provide encoders using ISO formatters (same as server default behavior)
  implicit val instantEncoder: ArgEncoder[Instant] = temporalEncoder(_.toString)
  implicit val localDateEncoder: ArgEncoder[LocalDate] = temporalEncoder(_.toString)
  implicit val localTimeEncoder: ArgEncoder[LocalTime] = temporalEncoder(_.toString)
  implicit val localDateTimeEncoder: ArgEncoder[LocalDateTime] = temporalEncoder(_.toString)
  implicit val offsetTimeEncoder: ArgEncoder[OffsetTime] = temporalEncoder(_.toString)
  implicit val offsetDateTimeEncoder: ArgEncoder[OffsetDateTime] = temporalEncoder(_.toString)
  implicit val zonedDateTimeEncoder: ArgEncoder[ZonedDateTime] = temporalEncoder(_.toString)
}
