package caliban.client

import zio.test._
import java.util.UUID

object ArgEncoderSpec extends ZIOSpecDefault {
  override def spec =
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
      ),
      suite("__UUIDValue")(
        test("regular uuid") {
          assertTrue(
            ArgEncoder.uuid
              .encode(UUID.fromString("20a69d87-6d68-4779-a4da-601f4c04ebf3"))
              .toString == """"20a69d87-6d68-4779-a4da-601f4c04ebf3""""
          )
        }
      )
    )
}
