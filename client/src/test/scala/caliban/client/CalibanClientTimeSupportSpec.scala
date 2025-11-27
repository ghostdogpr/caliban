package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.__Value._
import zio.test._

import java.time._
import java.time.format.DateTimeFormatter

object CalibanClientTimeSupportSpec extends ZIOSpecDefault {

  // Helper methods to summon and use the implicits from companion objects
  private def encode[A: ArgEncoder](value: A): __Value =
    implicitly[ArgEncoder[A]].encode(value)

  private def decode[A: ScalarDecoder](value: __Value): Either[DecodingError, A] =
    implicitly[ScalarDecoder[A]].decode(value)

  override def spec =
    suite("CalibanClientTimeSupportSpec")(
      suite("Instant")(
        test("encodes an Instant to a __StringValue") {
          val instant = Instant.parse("2023-01-15T10:30:00Z")
          assertTrue(encode(instant) == __StringValue("2023-01-15T10:30:00Z"))
        },
        test("decodes a valid string to an Instant") {
          val value = __StringValue("2023-01-15T10:30:00Z")
          assertTrue(decode[Instant](value) == Right(Instant.parse("2023-01-15T10:30:00Z")))
        },
        test("fails to decode an invalid string for Instant") {
          val value = __StringValue("not-an-instant")
          assert(decode[Instant](value))(Assertion.isLeft(Assertion.isSubtype[DecodingError](Assertion.anything)))
        }
      ),
      suite("LocalDate")(
        test("encodes a LocalDate to a __StringValue") {
          val localDate = LocalDate.of(2023, 1, 15)
          assertTrue(encode(localDate) == __StringValue("2023-01-15"))
        },
        test("decodes a valid string to a LocalDate") {
          val value = __StringValue("2023-01-15")
          assertTrue(decode[LocalDate](value) == Right(LocalDate.of(2023, 1, 15)))
        }
      ),
      suite("LocalTime")(
        test("encodes a LocalTime to a __StringValue") {
          val localTime = LocalTime.of(10, 30, 0)
          assertTrue(encode(localTime) == __StringValue("10:30"))
        },
        test("decodes a valid string to a LocalTime") {
          val value = __StringValue("10:30:00")
          assertTrue(decode[LocalTime](value) == Right(LocalTime.of(10, 30, 0)))
        }
      ),
      suite("LocalDateTime")(
        test("encodes a LocalDateTime to a __StringValue") {
          val ldt = LocalDateTime.of(2023, 1, 15, 10, 30, 0)
          assertTrue(encode(ldt) == __StringValue("2023-01-15T10:30"))
        },
        test("decodes a valid string to a LocalDateTime") {
          val value = __StringValue("2023-01-15T10:30:00")
          assertTrue(decode[LocalDateTime](value) == Right(LocalDateTime.of(2023, 1, 15, 10, 30, 0)))
        }
      ),
      suite("OffsetTime")(
        test("encodes an OffsetTime to a __StringValue") {
          val offsetTime = OffsetTime.of(10, 30, 0, 0, ZoneOffset.ofHours(2))
          assertTrue(encode(offsetTime) == __StringValue("10:30+02:00"))
        },
        test("decodes a valid string to an OffsetTime") {
          val value = __StringValue("10:30:00+02:00")
          assertTrue(decode[OffsetTime](value) == Right(OffsetTime.of(10, 30, 0, 0, ZoneOffset.ofHours(2))))
        }
      ),
      suite("OffsetDateTime")(
        test("encodes an OffsetDateTime to a __StringValue") {
          val odt = OffsetDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneOffset.ofHours(-5))
          assertTrue(encode(odt) == __StringValue("2023-01-15T10:30-05:00"))
        },
        test("decodes a valid string to an OffsetDateTime") {
          val value = __StringValue("2023-01-15T10:30:00-05:00")
          assertTrue(
            decode[OffsetDateTime](value) == Right(
              OffsetDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneOffset.ofHours(-5))
            )
          )
        }
      ),
      suite("ZonedDateTime")(
        test("encodes a ZonedDateTime to a __StringValue") {
          val zdt = ZonedDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneId.of("Europe/Paris"))
          assertTrue(encode(zdt) == __StringValue("2023-01-15T10:30+01:00[Europe/Paris]"))
        },
        test("decodes a valid string to a ZonedDateTime") {
          val value = __StringValue("2023-01-15T10:30:00+01:00[Europe/Paris]")
          assertTrue(
            decode[ZonedDateTime](value) == Right(
              ZonedDateTime.of(2023, 1, 15, 10, 30, 0, 0, ZoneId.of("Europe/Paris"))
            )
          )
        }
      ),
      suite("Decoder Failure Modes")(
        test("fails to decode from a non-string value") {
          val value = __Value.__NumberValue(123)
          assert(decode[LocalDate](value)) {
            Assertion.isLeft(
              Assertion.hasField(
                "message",
                (e: DecodingError) => e.getMessage,
                Assertion.equalTo("Decoding Error: Can't build LocalDate from input 123")
              )
            )
          }
        },
        test("fails to decode with a clear error message for invalid format") {
          val value = __StringValue("invalid-date")
          assert(decode[LocalDate](value)) {
            Assertion.isLeft(
              Assertion.hasField(
                "message",
                (e: DecodingError) => e.getMessage,
                Assertion.startsWithString("Decoding Error: Can't build LocalDate from input invalid-date")
              )
            )
          }
        }
      ),
      suite("Custom Formats")(
        test("can encode and decode a LocalDate with a custom format") {
          val customFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
          val date            = LocalDate.of(2025, 7, 26)
          val dateAsString    = "26-07-2025"

          // 1. Define local implicits using the NEW, more convenient helper functions.
          implicit val customEncoder: ArgEncoder[LocalDate] =
            ArgEncoder.temporalEncoder(customFormatter)

          // Pass the standard LocalDate.parse method reference, which matches the expected signature.
          implicit val customDecoder: ScalarDecoder[LocalDate] =
            ScalarDecoder.temporalDecoder("LocalDate", customFormatter)(LocalDate.parse)

          // 2. Test the custom encoder and decoder
          val encoded = encode(date)
          val decoded = decode[LocalDate](__StringValue(dateAsString))

          assertTrue(encoded == __StringValue(dateAsString)) &&
          assertTrue(decoded == Right(date))
        }
      )
    )
}
