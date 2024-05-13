package caliban.schema

import caliban.Value.StringValue
import caliban._
import caliban.introspection.adt.{ __DeprecatedArgs, __Field, __Type, __TypeKind }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import play.api.libs.json.JsValue
import zio._
import zio.query.ZQuery
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

import java.util.UUID
import scala.concurrent.Future

object SchemaSpec extends ZIOSpecDefault {

  override def spec =
    suite("SchemaSpec")(
      test("effectful field") {
        assert(introspect[EffectfulFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
        )
      },
      test("effectful field as non-nullable") {
        assert(introspect[EffectfulFieldSchema].fields(__DeprecatedArgs()).toList.flatten.apply(1)._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL))
        )
      },
      test("infallible effectful field") {
        assert(introspect[InfallibleFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
        )
      },
      test("infallible effectful field as nullable") {
        assert(introspect[InfallibleFieldSchema].fields(__DeprecatedArgs()).toList.flatten.apply(1)._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR))
        )
      },
      test("tricky case with R") {
        case class Field(value: ZQuery[Console, Nothing, String])
        case class Queries(field: ZQuery[Clock, Nothing, Field])
        object MySchema extends GenericSchema[Console with Clock] {
          import auto._
          implicit lazy val queriesSchema: Schema[Console with Clock, Queries] = genAll
        }
        assert(MySchema.queriesSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
        )
      },
      test("field with Future") {
        assert(introspect[FutureFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
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
        object consoleSchema extends GenericSchema[Console] {

          case class A(s: String)
          object A {
            implicit val aSchema: Schema[Console, A] = gen
          }
          case class B(a: List[Option[A]])

          A.aSchema.toType_()

          val schema: Schema[Console, B] = gen
        }

        assert(Types.collectTypes(consoleSchema.schema.toType_()).map(_.name.getOrElse("")))(
          not(contains("SomeA")) && not(contains("OptionA")) && not(contains("None"))
        )
      },
      test("UUID field should be converted to ID") {
        assert(introspect[IDSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, String]("id", _.ofType.flatMap(_.name).get, equalTo("ID")))
        )
      },
      test("interface only take fields that return the same type") {
        assertTrue(introspect[MyInterface].fields(__DeprecatedArgs()).toList.flatten.map(_.name) == List("c1", "c2"))
      },
      test("excluding fields from interfaces") {
        assertTrue(
          introspect[InterfaceWithExclusions].fields(__DeprecatedArgs()).toList.flatten.map(_.name) == List("c1")
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

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      },
      test("field with Json object [play]") {
        import caliban.interop.play.json._
        case class Queries(to: JsValue, from: JsValue => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      },
      test("ZStream in a Query returns a list type") {
        case class Query(a: ZStream[Any, Throwable, Int])

        assertTrue(
          introspect[Query].fields(__DeprecatedArgs()).flatMap(_.headOption).map(_._type.kind).get == __TypeKind.LIST
        )
      },
      test("ZStream in a Subscription doesn't return a list type") {
        case class Query(a: ZStream[Any, Throwable, Int])

        assertTrue(
          introspectSubscription[Query]
            .fields(__DeprecatedArgs())
            .flatMap(_.headOption)
            .map(_._type.kind)
            .get == __TypeKind.SCALAR
        )
      },
      test("rename") {
        case class Something(b: Int)
        case class Query(something: Something)

        implicit val somethingSchema: Schema[Any, Something] = Schema.gen[Any, Something].rename("SomethingElse")

        assertTrue(introspectSubscription[Something].innerType.name.get == "SomethingElse")
      },
      test("union redirect") {
        case class Queries(union: RedirectingUnion)

        implicit val queriesSchema: Schema[Any, Queries] = genAll

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

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Int"))))
        )
      },
      test("value type scalar") {
        @GQLValueType(isScalar = true)
        case class Wrapper(value: Int)
        case class Queries(test: Option[Wrapper])

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
          isSome(hasField[__Type, Option[String]]("name", _.name, equalTo(Some("Wrapper"))))
        )
      },
      test("GQLExcluded") {
        case class Bar(value: String)
        case class Foo(foo: String, @GQLExcluded bar: Bar)
        case class QueryType(a: String, @GQLExcluded b: String, foo: Foo)
        case class Query(query: QueryType)
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

        case class Query(a: A, b: B)

        val interfaceType = Schema.gen[Any, Interface].toType_()

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
      test("enum supported directives") {
        sealed trait MyEnum
        object MyEnum {
          @GQLDirective(Directive("join__Graph", Map("name" -> StringValue("A")))) case object A extends MyEnum
          @GQLDirective(Directive("join__Graph", Map("name" -> StringValue("B")))) case object B extends MyEnum
        }
        case class Queries(myEnum: MyEnum)
        implicit val queriesSchema: Schema[Any, Queries] = genAll

        assertTrue(
          Types
            .collectTypes(introspect[Queries])
            .find(_.name contains "MyEnum")
            .flatMap(_.enumValues(__DeprecatedArgs()))
            .map(_.flatMap(_.directives.toList.flatten))
            .get == List(
            Directive("join__Graph", Map("name" -> StringValue("A"))),
            Directive("join__Graph", Map("name" -> StringValue("B")))
          )
        )
      },
      test("render can be called with a Schema where R is any") {
        assertTrue(caliban.render[EffectfulFieldSchema].nonEmpty)
      },
      test("render can be called with a Schema where R is not any") {
        object schema extends GenericSchema[Env]
        import schema.auto._
        assertTrue(caliban.renderWith[Env, EnvironmentSchema].nonEmpty)
      },
      test("renderSchema can be called with a Schema where R is any") {
        assertTrue(caliban.renderSchema[EffectfulFieldSchema, Unit, Unit]().nonEmpty)
      },
      test("renderSchema can be called with a Schema where R is not any") {
        object schema extends GenericSchema[Env]
        import schema.auto._
        assertTrue(caliban.renderSchemaWith[Env, EnvironmentSchema, Unit, Unit]().nonEmpty)
      },
      test("renderSchema should not produce duplicated types") {
        object schema extends GenericSchema[Env]
        import schema.auto._
        val expected = """schema {
                         |  query: EnvironmentSchema
                         |  mutation: Mutation
                         |}
                         |
                         |type Box {
                         |  value: Int!
                         |}
                         |
                         |type EnvironmentSchema {
                         |  test: Int
                         |  box: Box!
                         |}
                         |
                         |type Mutation {
                         |  mutBox(value: Int!): Box!
                         |}""".stripMargin
        assertTrue(caliban.renderSchemaWith[Env, EnvironmentSchema, Mutation, Unit]() == expected)
      },
      test("custom enum schema") {

        case class Query(myEnum: EnumLikeUnion)

        implicit val myEnumSchema: Schema[Any, EnumLikeUnion] =
          enumSchema("Foo", Some("foo description"), List(enumValue("A")), repr = _.toString)

        val gql = graphQL(RootResolver(Query(EnumLikeUnion.A)))
        assertTrue(
          gql.render ==
            """schema {
              |  query: Query
              |}
              |
              |"foo description"
              |enum Foo {
              |  A
              |}
              |
              |type Query {
              |  myEnum: Foo!
              |}""".stripMargin
        )
      },
      test("nested interfaces") {
        case class Query(top: NestedInterface, mid1: NestedInterface.Mid1, mid2: NestedInterface.Mid2)

        val v      = NestedInterface.FooB("b", "c", "d")
        val schema = graphQL(RootResolver(Query(v, v, v))).render

        assertTrue(
          schema == """schema {
                      |  query: Query
                      |}
                      |
                      |interface Mid1 {
                      |  b: String!
                      |  c: String!
                      |}
                      |
                      |interface Mid2 {
                      |  b: String!
                      |  d: String!
                      |}
                      |
                      |interface NestedInterface {
                      |  b: String!
                      |}
                      |
                      |type FooA implements NestedInterface & Mid1 {
                      |  a: String!
                      |  b: String!
                      |  c: String!
                      |}
                      |
                      |type FooB implements NestedInterface & Mid1 & Mid2 {
                      |  b: String!
                      |  c: String!
                      |  d: String!
                      |}
                      |
                      |type FooC implements NestedInterface & Mid2 {
                      |  b: String!
                      |  d: String!
                      |  e: String!
                      |}
                      |
                      |type Query {
                      |  top: NestedInterface!
                      |  mid1: Mid1!
                      |  mid2: Mid2!
                      |}""".stripMargin
        )
      }
    )

  case class EffectfulFieldSchema(q: Task[Int], @GQLNonNullable qAnnotated: Task[Int])
  case class OptionalEffectfulFieldSchema(q: Task[Option[String]], @GQLNonNullable qAnnotated: Task[Option[String]])
  case class InfallibleFieldSchema(q: UIO[Int], @GQLNullable qAnnotated: UIO[Int])
  case class FutureFieldSchema(q: Future[Int])
  case class IDSchema(id: UUID)
  trait Env
  case class Box(value: Int)
  case class EnvironmentSchema(test: RIO[Env, Int], box: Box)
  case class Mutation(mutBox: Int => Box)

  @GQLInterface
  sealed trait MyInterface
  object MyInterface             {
    case class A(c1: Int, c2: Int => Int, d1: Int, d2: Int => Int, d3: Int => Int)      extends MyInterface
    case class B(c1: Int, c2: Int => Int, d1: Boolean, d2: Int, d3: Option[Int] => Int) extends MyInterface
  }

  @GQLInterface(excludedFields = "c2", "c3")
  sealed trait InterfaceWithExclusions
  object InterfaceWithExclusions {
    case class A(c1: Int, c2: String, c3: Int, c5: Int) extends InterfaceWithExclusions
    case class B(c1: Int, c2: String, c3: Int, c4: Int) extends InterfaceWithExclusions
  }

  @GQLUnion
  sealed trait EnumLikeUnion
  object EnumLikeUnion           {
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

  @GQLInterface
  sealed trait NestedInterface

  object NestedInterface {

    @GQLInterface
    sealed trait Mid1 extends NestedInterface

    @GQLInterface
    sealed trait Mid2 extends NestedInterface

    case class FooA(a: String, b: String, c: String) extends Mid1
    case class FooB(b: String, c: String, d: String) extends Mid1 with Mid2
    case class FooC(b: String, d: String, e: String) extends Mid2
  }
}
