package caliban.schema

import java.time.{ Instant, LocalDate, LocalDateTime, OffsetDateTime, OffsetTime, ZoneOffset, ZonedDateTime }

import caliban.Value.{ IntValue, StringValue }
import zio.test._
import Assertion._

object ArgBuilderSpec extends DefaultRunnableSpec {
  def spec = suite("ArgBuilder")(
    suite("orElse")(
      test("handles failures")(
        assert((ArgBuilder.instant orElse ArgBuilder.instantEpoch).build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("short-circuits")(
        assert((ArgBuilder.instantEpoch orElse ArgBuilder.instant).build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      )
    ),
    suite("java.time")(
      test("Instant from epoch")(
        assert(ArgBuilder.instantEpoch.build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("Instant from string")(
        assert(ArgBuilder.instant.build(StringValue("1970-01-01T00:00:00.100Z")))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("LocalDate from string")(
        assert(ArgBuilder.localDate.build(StringValue("1970-01-01")))(
          isRight(equalTo(LocalDate.of(1970, 1, 1)))
        )
      ),
      test("LocalDateTime from string")(
        assert(ArgBuilder.localDateTime.build(StringValue("1970-01-01T01:30")))(
          isRight(equalTo(LocalDateTime.of(1970, 1, 1, 1, 30)))
        )
      ),
      test("OffsetTime from string")(
        assert(ArgBuilder.offsetTime.build(StringValue("01:02:03.000Z")))(
          isRight(equalTo(OffsetTime.of(1, 2, 3, 0, ZoneOffset.UTC)))
        )
      ),
      test("OffsetDateTime from string")(
        assert(ArgBuilder.offsetDateTime.build(StringValue("1970-01-01T01:01:01Z")))(
          isRight(equalTo(OffsetDateTime.of(1970, 1, 1, 1, 1, 1, 0, ZoneOffset.UTC)))
        )
      ),
      test("ZonedDateTime from string")(
        assert(ArgBuilder.zonedDateTime.build(StringValue("1970-01-01T00:00:00.100Z")))(
          isRight(equalTo(ZonedDateTime.ofInstant(Instant.ofEpochMilli(100), ZoneOffset.UTC)))
        )
      )
    )
  )
}
