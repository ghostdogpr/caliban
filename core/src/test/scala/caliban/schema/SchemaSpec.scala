package caliban.schema

import java.util.UUID
import caliban.introspection.adt.{ __DeprecatedArgs, __Type, __TypeKind }
import caliban.schema.Annotations.{ GQLInterface, GQLUnion, GQLValueType }
import zio.blocking.Blocking
import zio.console.Console
import zio.query.ZQuery
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{ Task, UIO }

import scala.concurrent.Future

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
      test("tricky case with R") {
        case class Field(value: ZQuery[Console, Nothing, String])
        case class Queries(field: ZQuery[Blocking, Nothing, Field])
        object MySchema extends GenericSchema[Console with Blocking] {
          implicit lazy val queriesSchema: Schema[Console with Blocking, Queries] = gen
        }
        assert(MySchema.queriesSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
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
      test("nested types with explicit schema in companion object") {
        object blockingSchema extends GenericSchema[Blocking] {

          case class A(s: String)
          object A {
            implicit val aSchema: Schema[Blocking, A] = gen
          }
          case class B(a: List[Option[A]])

          A.aSchema.toType_()

          val schema: Schema[Blocking, B] = gen
        }

        assert(Types.collectTypes(blockingSchema.schema.toType_()).map(_.name.getOrElse("")))(
          not(contains("SomeA")) && not(contains("OptionA")) && not(contains("None"))
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
      test("enum-like sealed traits annotated with GQLUnion") {
        assert(introspect[EnumLikeUnion])(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.UNION))
        )
      },
      test("enum-like sealed traits annotated with GQLInterface") {
        assert(introspect[EnumLikeInterface])(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.INTERFACE))
        )
      },
      test("field with Json object [circe]") {
        import caliban.interop.circe.json._
        case class Queries(to: io.circe.Json, from: io.circe.Json => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      },
      test("ZStream in a Query returns a list type") {
        case class Query(a: ZStream[Any, Throwable, Int])

        assert(introspect[Query].fields(__DeprecatedArgs()).flatMap(_.headOption).map(_.`type`().kind))(
          isSome(equalTo(__TypeKind.LIST))
        )
      },
      test("ZStream in a Subscription doesn't return a list type") {
        case class Query(a: ZStream[Any, Throwable, Int])

        assert(introspectSubscription[Query].fields(__DeprecatedArgs()).flatMap(_.headOption).map(_.`type`().kind))(
          isSome(equalTo(__TypeKind.SCALAR))
        )
      },
      test("rename") {
        case class Something(b: Int)
        case class Query(something: Something)

        implicit val somethingSchema: Schema[Any, Something] = Schema.gen[Any, Something].rename("SomethingElse")

        assert(Types.innerType(introspectSubscription[Something]).name)(isSome(equalTo("SomethingElse")))
      },
      test("union redirect") {
        case class Queries(union: RedirectingUnion)

        implicit val queriesSchema: Schema[Any, Queries] = Schema.gen

        val types      = Types.collectTypes(introspect[Queries])
        val subTypes   = types.find(_.name.contains("RedirectingUnion")).flatMap(_.possibleTypes)
        val fieldNames =
          subTypes.toList.flatMap(_.flatMap(_.fields(__DeprecatedArgs()).map(_.map(_.name)))).toSet.flatten
        assert(subTypes.map(_.flatMap(_.name)))(
          isSome(
            hasSameElements(
              List("A", "B")
            )
          )
        ) &&
        assert(fieldNames)(hasSameElements(List("common")))
      },
      test("value type not scalar") {
        @GQLValueType
        case class Wrapper(value: Int)
        case class Queries(test: Option[Wrapper])

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Int"))))
        )
      },
      test("value type scalar") {
        @GQLValueType(isScalar = true)
        case class Wrapper(value: Int)
        case class Queries(test: Option[Wrapper])

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Wrapper"))))
        )
      }
    )

  case class EffectfulFieldSchema(q: Task[Int])
  case class InfallibleFieldSchema(q: UIO[Int])
  case class FutureFieldSchema(q: Future[Int])
  case class IDSchema(id: UUID)

  @GQLInterface
  sealed trait MyInterface
  object MyInterface   {
    case class A(common: Int, different: String)  extends MyInterface
    case class B(common: Int, different: Boolean) extends MyInterface
  }

  @GQLUnion
  sealed trait EnumLikeUnion
  object EnumLikeUnion {
    case object A extends EnumLikeUnion
    case object B extends EnumLikeUnion
  }

  @GQLUnion
  sealed trait RedirectingUnion

  object RedirectingUnion  {
    case class B(common: Int)

    case class A(common: Int) extends RedirectingUnion

    @GQLValueType
    case class Redirect(value: B) extends RedirectingUnion
  }

  @GQLInterface
  sealed trait EnumLikeInterface
  object EnumLikeInterface {
    case object A extends EnumLikeInterface
    case object B extends EnumLikeInterface
  }

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type             = schema.toType_()
  def introspectSubscription[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType_(isSubscription = true)
}
