package caliban

import caliban.Value.BooleanValue
import caliban.Value.FloatValue
import caliban.Value.FloatValue.{ BigDecimalNumber, DoubleNumber }
import caliban.Value.IntValue.IntNumber
import zio.test.{ assertCompletes, assertTrue, ZIOSpecDefault }

object ValueSpec extends ZIOSpecDefault {

  val spec = suite("ValueSpec")(
    suite("IntNumber")(
      test("uses cached instances for ints in the 0 <= x <= 1023 range") {
        (0 to 1023).map { i =>
          val x = IntNumber(i)
          val y = IntNumber(i)
          assertTrue(x eq y)
        }.foldLeft(assertCompletes)(_ && _)
      }
    ),
    suite("BooleaValue")(
      test("uses cached instance for true") {
        val x = BooleanValue(true)
        val y = BooleanValue(true)
        assertTrue(x eq y)
      },
      test("uses cached instance for false") {
        val x = BooleanValue(false)
        val y = BooleanValue(false)
        assertTrue(x eq y)
      }
    ),
    suite("FloatValue.fromStringUnsafe")(
      test("preserves overflowing values as BigDecimal instead of Infinity") {
        val v = FloatValue.fromStringUnsafe("1e400")
        assertTrue(
          v == BigDecimalNumber(BigDecimal("1e400")),
          v.toBigDecimal == BigDecimal("1e400"),
          v.toString != "Infinity"
        )
      },
      test("preserves underflowing values as BigDecimal instead of zero") {
        val v = FloatValue.fromStringUnsafe("1e-400")
        assertTrue(
          v == BigDecimalNumber(BigDecimal("1e-400")),
          v.toBigDecimal == BigDecimal("1e-400")
        )
      },
      test("keeps in-range values as Double") {
        assertTrue(FloatValue.fromStringUnsafe("1.5") == DoubleNumber(1.5))
      },
      test("keeps genuine zero as Double") {
        assertTrue(FloatValue.fromStringUnsafe("0.0") == DoubleNumber(0.0))
      }
    )
  )
}
