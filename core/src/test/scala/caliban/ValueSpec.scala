package caliban

import caliban.Value.BooleanValue
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
    )
  )
}
