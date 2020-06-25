package caliban

import zio.test.AssertionM.Render.param
import zio.test.{ Assertion, Eql }

package object tools {

  def normalize(value: String): String =
    value
      .split("\n")
      .collect {
        case string if string.nonEmpty || string.trim.nonEmpty => string.trim
      }
      .mkString("\n")

  def stringEqualTo[A, B](expected: String)(implicit eql: Eql[String, String]): Assertion[String] =
    Assertion.assertion("equalTo")(param(expected))(actual => normalize(actual) == normalize(expected))

}
