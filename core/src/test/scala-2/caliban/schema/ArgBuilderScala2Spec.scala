package caliban.schema

import caliban.Value.StringValue
import caliban.schema.ArgBuilder.auto._
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object ArgBuilderScala2Spec extends ZIOSpecDefault {

  case class RecursionTest(
    id: String,
    next: Option[RecursionTest]
  )

  def spec = suite("ArgBuilderScala2")(
    suite("AnyVal")(
      test("ArgBuilder that extends AnyVal") {
        val id    = UUID.randomUUID()
        val value = ArgBuilder[UUIDId].build(StringValue(id.toString))
        assert(value)(isRight(equalTo(UUIDId(id))))
      }
    ),
    suite("Recursion")(
      test("Should support recursion") {
        val argBuilder: ArgBuilder[RecursionTest] = ArgBuilder.gen
        assert(argBuilder)(Assertion.anything)
      }
    )
  )

  trait Ids[T] extends Any {
    self: AnyVal =>
    def value: T
  }

  final case class UUIDId(value: UUID) extends AnyVal with Ids[UUID]
}
