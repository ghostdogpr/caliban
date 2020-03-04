package caliban.parsing

import caliban.parsing.adt.LocationInfo
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object SourceMapperSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SourceMapper")(
      test("should not throw IndexOutOfBounds") {
        assert(SourceMapper("").getLocation(100))(equalTo(LocationInfo(101, 1)))
      },
      test("should map correctly to the source location") {
        val sm = SourceMapper("""
                                |a
                                |b
                                |""".stripMargin)
        assert(sm.getLocation(3))(equalTo(LocationInfo(1, 3)))
      }
    )
}
