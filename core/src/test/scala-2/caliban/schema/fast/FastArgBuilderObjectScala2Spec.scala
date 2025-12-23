package caliban.schema.fast

import caliban.InputValue
import zio.Scope
import zio.test._
import model._

object FastArgBuilderObjectScala2Spec extends ZIOSpecDefault {

  import util._
  import util.syntax._

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("FastArgBuilderObjectScala2")(
    suite("ClassWithZeroFields")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(gqlObject(), gqlObject("x" -> 10))

          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithZeroFields](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        }
      ),
      test("does not accept incorrect values") {
        val inputs = List[InputValue](10, gqlList(), "str")
        checkAll(Gen.fromIterable(inputs)) { input =>
          val got = invokeFastArgBuilder[ClassWithZeroFields](input)

          assertTrue(got.isLeft)
        }
      }
    ),
    suite("ClassWithOneField")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val input = gqlObject("myField" -> 10)

          val (expected, got) = invokeArgBuilders[ClassWithOneField](input)

          assertTrue(
            expected.isRight,
            expected == got
          )
        },
        test("on incorrect values") {
          val inputs = List(gqlObject(), gqlObject("notMyField" -> 10), gqlObject("myField" -> "not 10"))
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithOneField](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("ClassWithMultipleFields")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {

          val inputs = List(
            gqlObject("id" -> 10L, "age" -> 42, "name" -> "Alex", "kek" -> gqlObject("myField" -> 1)),
            gqlObject("id" -> 10, "age"  -> 42, "name" -> "Alex", "kek" -> gqlObject("myField" -> 1)),
            gqlObject("id" -> 10L, "age" -> 42, "name" -> gqlNull),
            gqlObject("id" -> 10L, "age" -> 42),
            gqlObject("id" -> 10L, "age" -> 42, "kek"  -> gqlNull)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithMultipleFields](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject(),
            gqlObject("id"  -> 10L),
            gqlObject("age" -> 42),
            gqlObject("id"  -> 10L, "age" -> 42, "kek" -> gqlObject())
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithMultipleFields](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("ClassWithRenamedField")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {

          val inputs = List(
            gqlObject("renamed" -> 10, "anotherField" -> 20)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithRenamedField](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject("myField" -> 10, "anotherField"    -> 20),
            gqlObject("renamed" -> "str", "anotherField" -> 20),
            gqlObject("renamed" -> 10)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithRenamedField](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("ClassWithDefaultValue")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(
            gqlObject("x" -> "str"),
            gqlObject("x" -> "str", "defaultObject"     -> gqlObject("myField" -> 1)),
            gqlObject("x" -> "str", "defaultList"       -> gqlList(1, 2, 3)),
            gqlObject("x" -> "str", "defaultInt"        -> 1),
            gqlObject("x" -> "str", "defaultLong"       -> 1),
            gqlObject("x" -> "str", "defaultBigInt"     -> 1),
            gqlObject("x" -> "str", "defaultFloat"      -> 1),
            gqlObject("x" -> "str", "defaultDouble"     -> 1),
            gqlObject("x" -> "str", "defaultBigDecimal" -> 1),
            gqlObject("x" -> "str", "defaultStr"        -> "yet another string"),
            gqlObject("x" -> "str", "defaultBoolean"    -> false)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithDefaultValue](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject("x" -> "str", "defaultObject"     -> gqlObject("incorrect" -> 1)),
            gqlObject("x" -> "str", "defaultObject"     -> gqlNull),
            gqlObject("x" -> "str", "defaultList"       -> gqlNull),
            gqlObject("x" -> "str", "defaultInt"        -> gqlNull),
            gqlObject("x" -> "str", "defaultLong"       -> gqlNull),
            gqlObject("x" -> "str", "defaultBigInt"     -> gqlNull),
            gqlObject("x" -> "str", "defaultFloat"      -> gqlNull),
            gqlObject("x" -> "str", "defaultDouble"     -> gqlNull),
            gqlObject("x" -> "str", "defaultBigDecimal" -> gqlNull),
            gqlObject("x" -> "str", "defaultStr"        -> gqlNull),
            gqlObject("x" -> "str", "defaultBoolean"    -> gqlNull)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithDefaultValue](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("ClassWithVarDefaultValue")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(
            gqlObject("x" -> "str", "defaultValue" -> 1)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithVarDefaultValue](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject("x" -> "str"),
            gqlObject("x" -> "str", "defaultValue" -> gqlNull)
          )
          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[ClassWithVarDefaultValue](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("ClassWithRecursion")(
      test("works with correct values (1)") {
        val input = gqlObject("x" -> 10)
        val got   = invokeFastArgBuilder[ClassWithRecursion](input)
        assertTrue(got.contains(ClassWithRecursion(10, None)))
      },
      test("works with correct values (2)") {
        val input = gqlObject("x" -> 10, "rest" -> gqlObject("x" -> 20))
        val got   = invokeFastArgBuilder[ClassWithRecursion](input)
        assertTrue(got.contains(ClassWithRecursion(10, Some(ClassWithRecursion(20, None)))))
      },
      test("works with correct values (3)") {
        val input =
          gqlObject("x" -> 10, "rest" -> gqlObject("x" -> 20, "rest" -> gqlObject("x" -> 30)))
        val got   = invokeFastArgBuilder[ClassWithRecursion](input)
        assertTrue(
          got.contains(ClassWithRecursion(10, Some(ClassWithRecursion(20, Some(ClassWithRecursion(30, None))))))
        )
      },
      test("works with incorrect values") {
        val inputs = List[InputValue](
          gqlObject(),
          10,
          "str"
        )
        checkAll(Gen.fromIterable(inputs)) { input =>
          val got = invokeFastArgBuilder[ClassWithRecursion](input)

          assertTrue(got.isLeft)
        }
      }
    ),
    suite("CalibanFoo")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(
            gqlObject(),
            gqlObject("value" -> "foo"),
            gqlObject("bar"   -> "foo")
          )

          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[CalibanFoo](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlNull
          )

          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[CalibanFoo](input)

            assertTrue(
              expected.isLeft,
              got.isLeft
            )
          }
        }
      )
    ),
    suite("CalibanMap")(
      suite("behaves as caliban.schema.ArgBuilder.gen")(
        test("on correct values") {
          val inputs = List(
            gqlObject("map" -> gqlList(gqlObject("key" -> "key", "value" -> "value")))
          )

          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[CalibanMap](input)

            assertTrue(
              expected.isRight,
              expected == got
            )
          }
        },
        test("on incorrect values") {
          val inputs = List(
            gqlObject("map" -> gqlNull),
            gqlObject("map" -> "foo"),
            gqlObject("map" -> gqlList("foo")),
            gqlObject("map" -> gqlList(gqlObject("key" -> "bar"))),
            gqlObject("map" -> gqlList(gqlObject("value" -> "bar"))),
            gqlObject("map" -> gqlList(gqlObject("key" -> "bar", "value" -> 1)))
          )

          checkAll(Gen.fromIterable(inputs)) { input =>
            val (expected, got) = invokeArgBuilders[CalibanMap](input)

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
