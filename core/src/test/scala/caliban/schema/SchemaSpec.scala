package caliban.schema

import java.util.UUID
import scala.concurrent.Future
import caliban.introspection.adt.{ __DeprecatedArgs, __Type, __TypeKind }
import caliban.schema.Annotations.GQLInterface
import play.api.libs.json.JsValue
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{ Task, UIO }

object SchemaSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SchemaSpec")(
      test("effectful field") {
        assert(introspect[EffectfulFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
        )
      },
      test("infallible effectful field") {
        assert(introspect[InfallibleFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
        )
      },
      test("field with Future") {
        assert(introspect[FutureFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
        )
      },
      test("nested input fields") {
        case class Queries(a: A => Unit)
        case class A(b: B)
        case class B(c: C)
        case class C(d: Int)
        assert(Types.collectTypes(introspect[Queries]).map(_.name.getOrElse("")))(
          contains("BInput") && contains("CInput")
        )
      },
      test("nested types") {
        case class Queries(a: Generic[Option[Double]], b: Generic[Option[Int]])
        case class Generic[T](value: T)
        assert(Types.collectTypes(introspect[Queries]).map(_.name.getOrElse("")))(
          contains("GenericOptionDouble") && contains("GenericOptionInt")
        )
      },
      test("UUID field should be converted to ID") {
        assert(introspect[IDSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("id", _.ofType.flatMap(_.name).get, equalTo("ID")))
        )
      },
      test("interface only take fields that return the same type") {
        assert(introspect[MyInterface].fields(__DeprecatedArgs()).toList.flatten.map(_.name))(
          equalTo(List("common"))
        )
      },
      test("field with Json object [circe]") {
        import caliban.interop.circe.json._
        case class Queries(to: io.circe.Json, from: io.circe.Json => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      },
      test("field with Json object [play]") {
        import caliban.interop.play.json._
        case class Queries(to: JsValue, from: JsValue => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      }
    )

  case class EffectfulFieldSchema(q: Task[Int])
  case class InfallibleFieldSchema(q: UIO[Int])
  case class FutureFieldSchema(q: Future[Int])
  case class IDSchema(id: UUID)

  @GQLInterface
  sealed trait MyInterface
  object MyInterface {
    case class A(common: Int, different: String)  extends MyInterface
    case class B(common: Int, different: Boolean) extends MyInterface
  }

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType()
}
