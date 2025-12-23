package caliban.schema.fast

import caliban.InputValue
import caliban.schema.fast.model._
import zio.Scope
import zio.test._

object FastArgBuilderOneOfScala2Spec extends ZIOSpecDefault {

  import util._
  import util.syntax._

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("FastArgBuilderOneOfScala2")(
    suite("SomeOneOf")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List[InputValue](
            gqlObject("id"    -> 1),
            gqlObject("idStr" -> "42"),
            gqlObject("field" -> gqlObject("myField" -> 42))
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[SomeOneOf](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List[InputValue](
            10,
            gqlObject("id"            -> 1, "field"         -> gqlObject("myField" -> 42)),
            gqlObject("id"            -> 1, "anything else" -> gqlNull),
            gqlObject("anything else" -> gqlNull),
            gqlObject("id"            -> "str"),
            gqlObject("idStr1"        -> "str"),
            gqlObject("idStr"         -> true),
            gqlObject("field"         -> "str"),
            gqlObject()
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[SomeOneOf](input)

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
