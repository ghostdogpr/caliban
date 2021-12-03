package caliban.client

import zio.test._
import zio.test.environment.TestEnvironment

object ArgEncoderSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ArgEncoderSpec")(
      suite("__StringValue")(
        test("regular string") {
          assertTrue(ArgEncoder.string.encode("abcde who am i?").toString == """"abcde who am i?"""")
        },
        test("string with quotes") {
          assertTrue(ArgEncoder.string.encode("abcde \"who am i?\"").toString == """"abcde \"who am i?\""""")
        },
        test("string with new line") {
          assertTrue(ArgEncoder.string.encode("abcde\n who\n am\n i\n").toString == """"abcde\n who\n am\n i\n"""")
        },
        test("string with null characters") {
          assertTrue(ArgEncoder.string.encode("abcde who am i\u0000").toString == "\"abcde who am i\\u0000\"")
        }
      )
    )
}
