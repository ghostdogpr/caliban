package caliban.schema

import java.util.UUID
import caliban._
import caliban.introspection.adt.{ __DeprecatedArgs, __Type, __TypeKind }
import caliban.schema.Annotations.{ GQLExcluded, GQLInterface, GQLUnion, GQLValueType }
import caliban.schema.Schema._
import caliban.schema.ArgBuilder.auto._
import zio.query.ZQuery
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio._

import scala.concurrent.Future

/** Most tests copied from SchemaSpec to test derivation via [[Schema.Auto]] */
object SchemaDerivesAutoSpec extends ZIOSpecDefault {

  override def spec =
    suite("Schema3DerivesAutoSpec")(
      test("effectful field") {
        case class EffectfulFieldSchema(q: Task[Int]) derives Schema.Auto

        assert(introspect[EffectfulFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
        )
      },
      test("infallible effectful field") {
        case class InfallibleFieldSchema(q: UIO[Int]) derives Schema.Auto

        assert(introspect[InfallibleFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
        )
      },
      test("tricky case with R") {
        case class Field(value: ZQuery[Console, Nothing, String])
        object MySchema extends GenericSchema[Console with Clock]
        case class Queries(field: ZQuery[Clock, Nothing, Field]) derives MySchema.Auto

        assert(
          summon[Schema[Console & Clock, Queries]]
            .toType_()
            .fields(__DeprecatedArgs())
            .toList
            .flatten
            .headOption
            .map(_.`type`())
        )(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
        )
      },
      test("field with Future") {
        case class FutureFieldSchema(q: Future[Int]) derives Schema.Auto

        assert(introspect[FutureFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
        )
      },
      test("nested input fields") {
        case class Queries(a: A => Unit) derives Schema.Auto
        case class A(b: B)
        case class B(c: C)
        case class C(d: Int)

        assert(Types.collectTypes(introspect[Queries]).map(_.name.getOrElse("")))(
          contains("BInput") && contains("CInput")
        )
      },
      test("nested types") {
        case class Queries(a: Generic[Option[Double]], b: Generic[Option[Int]]) derives Schema.Auto
        case class Generic[T](value: T)

        assert(Types.collectTypes(introspect[Queries]).map(_.name.getOrElse("")))(
          contains("GenericOptionDouble") && contains("GenericOptionInt")
        )
      },
      test("nested types with explicit schema in companion object") {
        object consoleSchema extends GenericSchema[Console] {
          case class A(s: String)
          object A {
            implicit val aSchema: Schema[Console, A] = gen
          }
          A.aSchema.toType_()
        }
        case class B(a: List[Option[consoleSchema.A]]) derives consoleSchema.Auto

        assert(Types.collectTypes(summon[Schema[Console, B]].toType_()).map(_.name.getOrElse("")))(
          not(contains("SomeA")) && not(contains("OptionA")) && not(contains("None"))
        )
      },
      test("UUID field should be converted to ID") {
        case class IDSchema(id: UUID) derives Schema.Auto

        assert(introspect[IDSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("id", _.ofType.flatMap(_.name).get, equalTo("ID")))
        )
      },
      test("interface only take fields that return the same type") {
        @GQLInterface
        sealed trait MyInterface derives Schema.Auto
        object MyInterface {
          case class A(common: Int, different: String)  extends MyInterface
          case class B(common: Int, different: Boolean) extends MyInterface
        }

        assertTrue(introspect[MyInterface].fields(__DeprecatedArgs()).toList.flatten.map(_.name) == List("common"))
      },
      test("enum-like sealed traits annotated with GQLUnion") {
        @GQLUnion
        sealed trait EnumLikeUnion derives Schema.Auto
        object EnumLikeUnion {
          case object A extends EnumLikeUnion
          case object B extends EnumLikeUnion
        }

        assert(introspect[EnumLikeUnion])(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.UNION))
        )
      },
      test("enum-like sealed traits annotated with GQLInterface") {
        @GQLInterface
        sealed trait EnumLikeInterface derives Schema.Auto
        object EnumLikeInterface {
          case object A extends EnumLikeInterface
          case object B extends EnumLikeInterface
        }

        assert(introspect[EnumLikeInterface])(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.INTERFACE))
        )
      },
      test("field with Json object [circe]") {
        import caliban.interop.circe.json._
        case class Queries(to: io.circe.Json, from: io.circe.Json => Unit) derives Schema.Auto

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      },
      test("ZStream in a Query returns a list type") {
        case class Query(a: ZStream[Any, Throwable, Int]) derives Schema.Auto

        assertTrue(
          introspect[Query].fields(__DeprecatedArgs()).flatMap(_.headOption).map(_.`type`().kind).get == __TypeKind.LIST
        )
      },
      test("ZStream in a Subscription doesn't return a list type") {
        case class Query(a: ZStream[Any, Throwable, Int]) derives Schema.Auto

        assertTrue(
          introspectSubscription[Query]
            .fields(__DeprecatedArgs())
            .flatMap(_.headOption)
            .map(_.`type`().kind)
            .get == __TypeKind.SCALAR
        )
      },
      test("rename") {
        case class Something(b: Int)
        case class Query(something: Something) derives Schema.Auto

        given Schema[Any, Something] = Schema.gen[Any, Something].rename("SomethingElse")

        assertTrue(Types.innerType(introspectSubscription[Something]).name.get == "SomethingElse")
      },
      test("union redirect") {
        @GQLUnion
        sealed trait RedirectingUnion
        object RedirectingUnion {
          case class B(common: Int)
          case class A(common: Int) extends RedirectingUnion

          @GQLValueType
          case class Redirect(value: B) extends RedirectingUnion
        }

        case class Queries(union: RedirectingUnion) derives Schema.Auto

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
        case class Queries(test: Option[Wrapper]) derives Schema.Auto

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Int"))))
        )
      },
      test("value type scalar") {
        @GQLValueType(isScalar = true)
        case class Wrapper(value: Int)
        case class Queries(test: Option[Wrapper]) derives Schema.Auto

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Wrapper"))))
        )
      },
      test("GQLExcluded") {
        case class Bar(value: String)
        case class Foo(foo: String, @GQLExcluded bar: Bar)
        case class QueryType(a: String, @GQLExcluded b: String, foo: Foo)
        case class Query(query: QueryType) derives Schema.Auto
        val gql      = graphQL(RootResolver(Query(QueryType("a", "b", Foo("foo", Bar("bar"))))))
        val expected = """schema {
                         |  query: Query
                         |}

                         |type Foo {
                         |  foo: String!
                         |}

                         |type Query {
                         |  query: QueryType!
                         |}

                         |type QueryType {
                         |  a: String!
                         |  foo: Foo!
                         |}""".stripMargin
        assertTrue(gql.render == expected)
      },
      test("Pass interface to withAdditionalTypes") {
        @GQLInterface
        sealed trait Interface

        case class A(s: String) extends Interface
        case class B(s: String) extends Interface

        case class Query(a: A, b: B) derives Schema.Auto

        val interfaceType = Schema.Auto.derived[Interface].toType_()

        val gql      = graphQL(RootResolver(Query(A("a"), B("b")))).withAdditionalTypes(List(interfaceType))
        val expected = """schema {
                         |  query: Query
                         |}
                         |
                         |interface Interface {
                         |  s: String!
                         |}
                         |
                         |type A implements Interface {
                         |  s: String!
                         |}
                         |
                         |type B implements Interface {
                         |  s: String!
                         |}
                         |
                         |type Query {
                         |  a: A!
                         |  b: B!
                         |}""".stripMargin
        assertTrue(gql.render == expected)
      },
      suite("Auto derivation reuses implicits") {
        val expected =
          """schema {
            |  query: Queries
            |}

            |type A {
            |  a: String!
            |}

            |type Queries {
            |  as: [A!]!
            |}""".stripMargin
        List(
          test("from GenericSchema") {
            case class A(a: String)
            case class Queries(as: List[A]) derives Schema.Auto

            val resolver = RootResolver(Queries(List(A("a"), A("b"))))
            val gql      = graphQL(resolver)

            assertTrue(gql.render == expected)
          },
          test("from local scope") {
            case class A(a: Int)

            given Schema[Any, A] = Schema.obj[Any, A]("A") { case given FieldAttributes =>
              List(field("a")(_.a.toString))
            }

            case class Queries(as: List[A]) derives Schema.Auto

            val resolver = RootResolver(Queries(List(A(1), A(2))))
            val gql      = graphQL(resolver)

            assertTrue(gql.render == expected)
          }
        )
      }
    )

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type             = schema.toType_()
  def introspectSubscription[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType_(isSubscription = true)
}
