package caliban.interop.fs2

import caliban.Value.IntValue
import caliban.interop.fs2.implicits._
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
import cats.effect.IO
import cats.effect.LiftIO
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits._
import fs2.Stream
import zio.Chunk
import zio.Exit
import zio.Task
import zio.Unsafe
import zio.ZIO
import zio.interop.catz._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable.Seq

object Fs2InteropSchemaSpec extends ZIOSpecDefault {
  case class Foo[F[_]](bar: Stream[F, Int])

  // Generates the Schema when the `IO` type is visible at compile time to the deriver.
  private def generateSchemaForIO(implicit dispatcher: Dispatcher[IO]): Schema[Any, Foo[IO]] =
    Schema.gen

  // Generates the Schema when a generic `F` type is visible at compile time to the deriver.
  private def generateSchemaForF[F[_]](implicit dispatcher: Dispatcher[F]): Schema[Any, Foo[F]] =
    Schema.gen

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
    },
    suiteAll("Stream of IO") {

      val testeeScoped =
        Dispatcher
          .sequential[IO]
          .map(generateSchemaForIO(_))
          .mapK(LiftIO.liftK[Task])
          .toScopedZIO

      test("should derive a correct schema type") {
        testeeScoped.map { testee =>
          assert(testee.toType_().toTypeDefinition) {
            streamDerivedAsListOfInts
          }
        }
      }
      test("should resolve to a correct stream step") {
        testeeScoped.flatMap { testee =>
          check(Gen.listOf(Gen.int)) { expectedValues =>
            val foo = Foo(Stream.emits(expectedValues).covary[IO])

            assert(testee.resolve(foo)) {
              streamResolvesToCorrectValues(expectedValues)
            }
          }
        }
      }
    },
    suiteAll("Stream of F") {
      val testeeScoped =
        Dispatcher
          .sequential[Task]
          .map(generateSchemaForF(_))
          .toScopedZIO

      test("should derive a correct schema type") {
        testeeScoped.map { testee =>
          assert(testee.toType_().toTypeDefinition) {
            streamDerivedAsListOfInts
          }
        }
      }
      test("should resolve to a correct stream step") {
        testeeScoped.flatMap { testee =>
          check(Gen.listOf(Gen.int)) { expectedValues =>
            val foo = Foo(Stream.emits(expectedValues).covary[Task])

            assert(testee.resolve(foo)) {
              streamResolvesToCorrectValues(expectedValues)
            }
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
