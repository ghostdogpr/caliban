package caliban.schema

import caliban._
import caliban.introspection.adt.{ __DeprecatedArgs, __Field, __Type, __TypeKind }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import zio._
import zio.test.Assertion._
import zio.test._

object SemanticNonNullSchema extends SchemaDerivation[Any] {
  override def config = DerivationConfig(enableSemanticNonNull = true)
}

object SemanticNonNullSchemaSpec extends ZIOSpecDefault {
  override def spec =
    suite("SemanticNonNullSchemaSpec")(
      test("effectful field as semanticNonNull") {
        val field = effectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(0)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives,
            isSome(contains(SchemaUtils.SemanticNonNull))
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR))
        )
      },
      test("effectful field as non-nullable") {
        val field = effectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(1)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives.map(_.filter(_ == SchemaUtils.SemanticNonNull)).filter(_.nonEmpty),
            isNone
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL))
        )
      },
      test("nullable effectful field") {
        val field = nullableEffectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(0)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives.map(_.filter(_ == SchemaUtils.SemanticNonNull)).filter(_.nonEmpty),
            isNone
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR))
        )
      },
      test("nullable effectful field as non-nullable") {
        val field = nullableEffectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(1)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives,
            isNone
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL))
        )
      },
      test("infallible effectful field") {
        val field = infallibleEffectfulFieldSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(0)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives,
            isNone
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL))
        )
      },
      test("infallible effectful field as nullable") {
        val field = infallibleEffectfulFieldSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.apply(1)
        assert(field)(
          hasField[__Field, Option[List[Directive]]](
            "directives",
            _.directives,
            isNone
          )
        )
        assert(field._type)(
          hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR))
        )
      }
    )

  case class EffectfulFieldObject(q: Task[Int], @GQLNonNullable qAnnotated: Task[Int])
  case class NullableEffectfulFieldObject(q: Task[Option[String]], @GQLNonNullable qAnnotated: Task[Option[String]])
  case class InfallibleFieldObject(q: UIO[Int], @GQLNullable qAnnotated: UIO[Int])

  implicit val effectfulFieldObjectSchema: Schema[Any, EffectfulFieldObject]                 =
    SemanticNonNullSchema.gen[Any, EffectfulFieldObject]
  implicit val nullableEffectfulFieldObjectSchema: Schema[Any, NullableEffectfulFieldObject] =
    SemanticNonNullSchema.gen[Any, NullableEffectfulFieldObject]
  implicit val infallibleEffectfulFieldSchema: Schema[Any, InfallibleFieldObject]            =
    SemanticNonNullSchema.gen[Any, InfallibleFieldObject]
}
