package caliban.parsing

import zio.test._
import Assertion._
import caliban.parsing.adt.LocationInfo

object SourceMapperSpec
    extends DefaultRunnableSpec(
      suite("SourceMapper")(
        test("should not throw IndexOutOfBounds") {
          assert(SourceMapper("").getLocation(100), equalTo(LocationInfo(101, 1)))
        },
        test("should map correctly to the source location") {
          val sm = SourceMapper("""
                                  |a
                                  |b
                                  |""".stripMargin)
          assert(sm.getLocation(3), equalTo(LocationInfo(1, 3)))
        }
      )
    )
