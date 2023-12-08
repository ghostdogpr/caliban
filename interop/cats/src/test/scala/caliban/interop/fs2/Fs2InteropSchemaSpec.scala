package caliban.interop.fs2

import caliban.Value.IntValue
import caliban.execution.Field
import caliban.interop.fs2.implicits._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, ObjectTypeDefinition }
import caliban.parsing.adt.Type._
import caliban.schema.Step.{ MetadataFunctionStep, ObjectStep, StreamStep }
import caliban.schema.{ PureStep, Schema, Step }
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits._
import cats.effect.{ IO, LiftIO }
import fs2.Stream
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Exit, Task, Unsafe, ZIO }

object Fs2InteropSchemaSpec extends ZIOSpecDefault {
  override def spec = suite("Fs2InteropSchemaSpec")(
    suite("Stream of Task")(
      makeTests(ZIO.succeed(generateSchemaForTask))
    ),
    suite("Stream of IO")(
      makeTests(
        Dispatcher
          .sequential[IO]
          .map(generateSchemaForIO(_))
          .mapK(LiftIO.liftK[Task])
          .toScopedZIO
      )
    ),
    suite("Stream of F")(
      makeTests(
        Dispatcher
          .sequential[Task]
          .map(generateSchemaForF(_))
          .toScopedZIO
      )
    )
  )

  private case class Foo[F[_]](bar: Stream[F, Int])

  // Generates the Schema when the `Task` type is visible at compile time to the deriver.
  private def generateSchemaForTask: Schema[Any, Foo[Task]] = Schema.gen

  // Generates the Schema when the `IO` type is visible at compile time to the deriver.
  private def generateSchemaForIO(implicit dispatcher: Dispatcher[IO]): Schema[Any, Foo[IO]] =
    Schema.gen

  // Generates the Schema when a generic `F` type is visible at compile time to the deriver.
  private def generateSchemaForF[F[_]](implicit dispatcher: Dispatcher[F]): Schema[Any, Foo[F]] =
    Schema.gen

  private def makeTests[F[_], R](testeeZIO: ZIO[R, Throwable, Schema[Any, Foo[F]]]) =
    Seq(
      test("should derive a correct schema type") {
        testeeZIO.map { testee =>
          assert(testee.toType_().toTypeDefinition) {
            streamDerivedAsListOfInts
          }
        }
      },
      test("should resolve to a correct stream step") {
        val field = new Field(
          "foo",
          __Type(__TypeKind.OBJECT),
          None,
          fields = List(new Field("bar", __Type(__TypeKind.LIST), None))
        )

        def resolveMetadataStep(step: Step[Any]): Step[Any] = step match {
          case MetadataFunctionStep(f) => f(field)
          case _                       => step
        }

        testeeZIO.flatMap { testee =>
          check(Gen.listOf(Gen.int)) { expectedValues =>
            val foo = Foo(Stream.emits(expectedValues).covary[F])

            assert(resolveMetadataStep(testee.resolve(foo))) {
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
        _.fields("bar").get,
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
  }
}
