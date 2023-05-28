package caliban.interop.fs2

import caliban.Value.IntValue
import caliban.interop.fs2.implicits._
import caliban.parsing.adt.Definition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.FieldDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.ObjectTypeDefinition
import caliban.parsing.adt.Type._
import caliban.schema.PureStep
import caliban.schema.Schema
import caliban.schema.Schema.auto._
import caliban.schema.Step
import caliban.schema.Step.ObjectStep
import caliban.schema.Step.StreamStep
import fs2.Stream
import zio.Chunk
import zio.Exit
import zio.Task
import zio.Unsafe
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable.Seq

object Fs2InteropSchemaSpec extends ZIOSpecDefault {
  case class Foo[F[_]](bar: Stream[F, Int])

  override def spec = suite("Fs2InteropSchemaSpec")(
    suiteAll("Stream of zio.Task") {
      val testee: Schema[Any, Foo[Task]] = Schema.gen

      test("should derive a correct schema type") {
        assert(testee.toType_().toTypeDefinition) {
          streamDerivedAsListOfInts
        }
      }
      test("should resolve to a correct stream step") {
        check(Gen.listOf(Gen.int)) { expectedValues =>
          val foo = Foo(Stream.emits(expectedValues).covary[Task])

          assert(testee.resolve(foo)) {
            streamResolvesToCorrectValues(expectedValues)
          }
        }
      }
    }
  )

  private val streamDerivedAsListOfInts: Assertion[Option[TypeDefinition]] =
    isSome(
      isSubtype[ObjectTypeDefinition](
        hasField(
          "fields",
          _.fields,
          hasFirst[FieldDefinition](
            hasField(
              "ofType",
              _.ofType,
              isSubtype[ListType](
                equalTo(
                  ListType(NamedType("Int", true), false)
                )
              )
            )
          )
        )
      )
    )

  private def streamResolvesToCorrectValues(expectedValues: Seq[Int]): Assertion[Step[Any]] = {
    val expectedChunk =
      Chunk
        .fromIterable(expectedValues)
        .map(v => PureStep(IntValue(v)): Step[Any])

    isSubtype[ObjectStep[Any]](
      hasField(
        "fields",
        _.fields,
        hasKey(
          "bar",
          isSubtype[StreamStep[Any]](
            hasField(
              "inner",
              step =>
                Unsafe.unsafe { implicit unsafe =>
                  runtime.unsafe.run(step.inner.runCollect)
                },
              isSubtype[Exit.Success[Chunk[Step[Any]]]](
                equalTo(Exit.Success(expectedChunk))
              )
            )
          )
        )
      )
    )
  }
}
