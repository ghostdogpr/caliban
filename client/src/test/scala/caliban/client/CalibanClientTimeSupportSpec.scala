package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.__Value
import caliban.client.__Value.__StringValue
import zio.test._
import java.time._

object CalibanClientTimeSupportSpec extends ZIOSpecDefault {

  // Import all the implicit encoders and decoders from your support object
  import CalibanClientTimeSupport._

  override def spec =
    suite("CalibanClientTimeSupportSpec")(
      suite("Instant")(
        test("encodes an Instant to a __StringValue") {
          val instant = Instant.parse("2023-01-15T10:30:00Z")
          assertTrue(instantEncoder.encode(instant) == __StringValue("2023-01-15T10:30:00Z"))
        },
        test("decodes a valid string to an Instant") {
          val value = __StringValue("2023-01-15T10:30:00Z")
          assertTrue(instantDecoder.decode(value) == Right(Instant.parse("2023-01-15T10:30:00Z")))
        },
        test("fails to decode an invalid string for Instant") {
          val value = __StringValue("not-an-instant")
          assert(instantDecoder.decode(value))(Assertion.isLeft(Assertion.isSubtype[DecodingError](Assertion.anything)))
        }
      ),
      suite("LocalDate")(
        test("encodes a LocalDate to a __StringValue") {
          val localDate = LocalDate.of(2023, 1, 15)
          assertTrue(localDateEncoder.encode(localDate) == __StringValue("2023-01-15"))
        },
        test("decodes a valid string to a LocalDate") {
          val value = __StringValue("2023-01-15")
          assertTrue(localDateDecoder.decode(value) == Right(LocalDate.of(2023, 1, 15)))
        }
      ),
      suite("LocalTime")(
        test("encodes a LocalTime to a __StringValue") {
          val localTime = LocalTime.of(10, 30, 0)
          assertTrue(localTimeEncoder.encode(localTime) == __StringValue("10:30"))
        },
        test("decodes a valid string to a LocalTime") {
          val value = __StringValue("10:30:00")
          assertTrue(localTimeDecoder.decode(value) == Right(LocalTime.of(10, 30, 0)))
        }
      ),
      suite("LocalDateTime")(
        test("encodes a LocalDateTime to a __StringValue") {
          val ldt = LocalDateTime.of(2023, 1, 15, 10, 30, 0)
          assertTrue(localDateTimeEncoder.encode(ldt) == __StringValue("2023-01-15T10:30"))
        },
        test("decodes a valid string to a LocalDateTime") {
          val value = __StringValue("2023-01-15T10:30:00")
          assertTrue(localDateTimeDecoder.decode(value) == Right(LocalDateTime.of(2023, 1, 15, 10, 30, 0)))
        }
      ),
      suite("OffsetTime")(
        test("encodes an OffsetTime to a __StringValue") {
          val offsetTime = OffsetTime.of(10, 30, 0, 0, ZoneOffset.ofHours(2))
          assertTrue(offsetTimeEncoder.encode(offsetTime) == __StringValue("10:30+02:00"))
        },
        test("decodes a valid string to an OffsetTime") {
          val value = __StringValue("10:30:00+02:00")
          assertTrue(offsetTimeDecoder.decode(value) == Right(OffsetTime.of(10, 30, 0, 0, ZoneOffset.ofHours(2))))
        }
      ),
      suite("OffsetDateTime")(
        test("encodes an OffsetDateTime to a __StringValue") {
          val odt = OffsetDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneOffset.ofHours(-5))
          assertTrue(offsetDateTimeEncoder.encode(odt) == __StringValue("2023-01-15T10:30-05:00"))
        },
        test("decodes a valid string to an OffsetDateTime") {
          val value = __StringValue("2023-01-15T10:30:00-05:00")
          assertTrue(
            offsetDateTimeDecoder.decode(value) == Right(OffsetDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneOffset.ofHours(-5)))
          )
        }
      ),
      suite("ZonedDateTime")(
        test("encodes a ZonedDateTime to a __StringValue") {
          val zdt = ZonedDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneId.of("Europe/Paris"))
          assertTrue(zonedDateTimeEncoder.encode(zdt) == __StringValue("2023-01-15T10:30+01:00[Europe/Paris]"))
        },
        test("decodes a valid string to a ZonedDateTime") {
          val value = __StringValue("2023-01-15T10:30:00+01:00[Europe/Paris]")
          assertTrue(
            zonedDateTimeDecoder.decode(value) == Right(ZonedDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneId.of("Europe/Paris")))
          )
        }
      ),
      suite("Decoder Failure Modes")(
        test("fails to decode from a non-string value") {
          val value = __Value.__NumberValue(123)
          assert(localDateDecoder.decode(value)) {
            Assertion.isLeft(
              Assertion.hasField("message", (e: DecodingError) => e.getMessage, Assertion.equalTo("Decoding Error: Can't build LocalDate from input 123"))
            )
          }
        },
        test("fails to decode with a clear error message for invalid format") {
           val value = __StringValue("invalid-date")
           assert(localDateDecoder.decode(value)) {
             Assertion.isLeft(
               Assertion.hasField("message", (e: DecodingError) => e.getMessage, Assertion.startsWithString("Decoding Error: Can't build LocalDate from input invalid-date"))
             )
           }
        }
      )
    )
}
