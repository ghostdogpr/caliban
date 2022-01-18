package caliban.client

import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment
import java.util.UUID

object ArgEncoderSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ArgEncoderSpec")(
      suite("__StringValue")(
        test("regular string") {
          assert(ArgEncoder.string.encode("abcde who am i?").toString)(equalTo(""""abcde who am i?""""))
        },
        test("string with quotes") {
          assert(ArgEncoder.string.encode("abcde \"who am i?\"").toString)(equalTo(""""abcde \"who am i?\"""""))
        },
        test("string with new line") {
          assert(ArgEncoder.string.encode("abcde\n who\n am\n i\n").toString)(equalTo(""""abcde\n who\n am\n i\n""""))
        },
        test("string with null characters") {
          assert(ArgEncoder.string.encode("abcde who am i\u0000").toString)(equalTo("\"abcde who am i\\u0000\""))
        }
      ),
      suite("__UUIDValue")(
        test("regular uuid") {
          assert(ArgEncoder.uuid.encode(UUID.fromString("20a69d87-6d68-4779-a4da-601f4c04ebf3")).toString)(
            equalTo(""""20a69d87-6d68-4779-a4da-601f4c04ebf3"""")
          )
        }
      )
    )
}
