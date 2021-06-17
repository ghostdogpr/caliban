package caliban

import caliban.CalibanError.ExecutionError
import caliban.GraphQL._
import caliban.InputValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.schema.Annotations.GQLInterface
import caliban.schema.ArgBuilder
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object Scala3SpecificSpec extends DefaultRunnableSpec {

  enum MyEnum {
    case A, B, C
  }

  enum MyADT {
    case A(a: Int)
    case B(b: String)
  }

  @GQLInterface
  enum MyADT2 {
    case A(a: Int)
    case B(a: Int)
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Scala3SpecificSpec")(
      testM("Scala 3 enum") {
        case class Queries(item: MyEnum)
        val api         = graphQL(RootResolver(Queries(MyEnum.A)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":"A"}""")
        )
      },
      testM("Scala 3 union") {
        case class Queries(item: MyADT)
        val api         = graphQL(RootResolver(Queries(MyADT.A(1))))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item {
            |     ... on A {
            |       a
            |     }
            |     ... on B {
            |       b
            |     }
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":{"a":1}}""")
        )
      },
      testM("Scala 3 interface") {
        case class Queries(item: MyADT2)
        val api         = graphQL(RootResolver(Queries(MyADT2.A(1))))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item {
            |    a
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":{"a":1}}""")
        )
      },
      test("Scala 3 buildMissing") {
        sealed abstract class Nullable[+T]
        case class SomeNullable[+T](t: T) extends Nullable[T]
        case object NullNullable          extends Nullable[Nothing]
        case object MissingNullable       extends Nullable[Nothing]

        implicit def nullableArgBuilder[A](implicit ev: ArgBuilder[A]): ArgBuilder[Nullable[A]] = new ArgBuilder[Nullable[A]] {
          def build(input: InputValue): Either[ExecutionError, Nullable[A]] = input match {
            case NullValue => Right(NullNullable)
            case _         => ev.build(input).map(SomeNullable(_))
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
