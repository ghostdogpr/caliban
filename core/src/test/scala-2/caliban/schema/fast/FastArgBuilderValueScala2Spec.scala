package caliban.schema.fast

import caliban.InputValue
import caliban.schema.fast.model._
import zio.Scope
import zio.test._

object FastArgBuilderValueScala2Spec extends ZIOSpecDefault {

  import util._
  import util.syntax._

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("FastArgBuilderValueScala2")(
    suite("SomeValueClass")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(
            gqlObject("myField" -> 42)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[SomeValueClass](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject(),
            gqlObject("notMyField" -> 10),
            gqlObject("value"      -> gqlObject("myField" -> 10)),
            gqlObject("value"      -> gqlNull)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[SomeValueClass](input)

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
