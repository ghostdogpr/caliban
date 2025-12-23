package caliban.schema.fast

import caliban.InputValue
import caliban.schema.ArgBuilder.auto._
import zio.test._

import model.MyEnum

object FastArgBuilderEnumScala2Spec extends ZIOSpecDefault {
  import util._
  import util.syntax._

  override def spec: Spec[TestEnvironment, Any] =
    suite("FastArgBuilderEnumScala2")(
      suite("MyEnum")(
        test("works with incorrect values") {
          val inputs = List[InputValue](
            10,
            "DOESNT_EXIST",
            "WHAT"
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val got = invokeFastArgBuilder[MyEnum](input)

            assertTrue(got.isLeft)
          }
        },
        suite("behaves as caliban.schema.ArgBuilder.gen")(
          test("on correct values") {
            val inputs = List[InputValue](
              "SOME_CASE",
              "ANOTHER_CASE",
              "RENAMED_CASE",
              gqlEnum("SOME_CASE"),
              gqlEnum("ANOTHER_CASE"),
              gqlEnum("RENAMED_CASE")
            )
            checkAll(Gen.fromIterable(inputs)) { input =>
              val (expected, got) = invokeArgBuilders[MyEnum](input)

              assertTrue(
                expected.isRight,
                expected == got
              )
            }
          },
          test("on incorrect values") {
            val inputs = List[InputValue](
              10,
              "DOESNT_EXIST"
            )
            checkAll(Gen.fromIterable(inputs)) { input =>
              val (expected, got) = invokeArgBuilders[MyEnum](input)

              assertTrue(
                expected.isLeft,
                got.isLeft
              )
            }
          }
        )
      )
    )

}
