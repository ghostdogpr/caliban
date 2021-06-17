package caliban

import caliban.CalibanError.ExecutionError
import caliban.GraphQL._
import caliban.InputValue.ObjectValue
import caliban.TestUtils._
import caliban.Value.{NullValue, StringValue}
import caliban.introspection.adt.__DeprecatedArgs
import caliban.schema.ArgBuilder
import caliban.schema.SchemaSpec.introspect
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object Scala2SpecificSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Scala2SpecificSpec")(
      test("value classes should unwrap") {
        case class Queries(organizationId: OrganizationId, painter: WrappedPainter)
        val fieldTypes = introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.map(_.`type`())
        assert(fieldTypes.map(_.ofType.flatMap(_.name)))(equalTo(Some("Long") :: Some("Painter") :: Nil))
      },
      testM("value classes") {
        case class Queries(events: List[Event], painters: List[WrappedPainter])
        val event       = Event(OrganizationId(7), "Frida Kahlo exhibition")
        val painter     = Painter("Claude Monet", "Impressionism")
        val api         = graphQL(RootResolver(Queries(event :: Nil, WrappedPainter(painter) :: Nil)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  events {
            |    organizationId
            |    title
            |  }
            |  painters {
            |    name
            |    movement
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"events":[{"organizationId":7,"title":"Frida Kahlo exhibition"}],"painters":[{"name":"Claude Monet","movement":"Impressionism"}]}"""
          )
        )
      },
      test("Scala 2 buildMissing") {
        sealed abstract class Nullable[+T]
        case class SomeNullable[+T](t: T) extends Nullable[T]
        case object NullNullable extends Nullable[Nothing]
        case object MissingNullable extends Nullable[Nothing]

        implicit def nullableArgBuilder[A](implicit ev: ArgBuilder[A]) = new ArgBuilder[Nullable[A]] {
          def build(input: InputValue): Either[ExecutionError, Nullable[A]] = input match {
            case NullValue => Right(NullNullable)
            case _ => ev.build(input).map(SomeNullable(_))
          }

          override def buildMissing: Either[ExecutionError, Nullable[A]] = Right(MissingNullable)
        }

        case class Wrapper(a: Nullable[String])

        val deriviedAB = implicitly[ArgBuilder[Wrapper]]

        assert(deriviedAB.build(ObjectValue(Map())))(equalTo(Right(Wrapper(MissingNullable)))) &&
          assert(deriviedAB.build(ObjectValue(Map("a" -> NullValue))))(equalTo(Right(Wrapper(NullNullable)))) &&
          assert(deriviedAB.build(ObjectValue(Map("a" -> StringValue("x")))))(equalTo(Right(Wrapper(SomeNullable("x")))))
      }
    )
}
