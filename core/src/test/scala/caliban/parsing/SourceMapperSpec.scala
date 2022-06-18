package caliban.parsing

import caliban.parsing.adt.LocationInfo
import zio.test._

object SourceMapperSpec extends ZIOSpecDefault {

  override def spec =
    suite("SourceMapper")(
      test("should not throw IndexOutOfBounds") {
        assertTrue(SourceMapper("").getLocation(100) == LocationInfo(101, 1))
      },
      test("should map correctly to the source location") {
        val sm = SourceMapper("""
                                |a
                                |b
                                |""".stripMargin)
        assertTrue(sm.getLocation(3) == LocationInfo(1, 3))
      }
    )
}
