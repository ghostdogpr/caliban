package caliban.schema

import caliban._
import caliban.introspection.adt.{ __DeprecatedArgs, __Field }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import zio._
import zio.test.Assertion._
import zio.test._

object SemanticNonNullSchema extends SchemaDerivation[Any] {
  override def enableSemanticNonNull: Boolean = true
}

object SemanticNonNullSchemaSpec extends ZIOSpecDefault {
  override def spec =
    suite("SemanticNonNullSchemaSpec")(
      test("effectful field as semanticNonNull") {
        assert(effectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.headOption)(
          isSome(
            hasField[__Field, Option[List[Directive]]](
              "directives",
              _.directives,
              isSome(contains((Directive("semanticNonNull"))))
            )
          )
        )
      },
      test("optional effectful field") {
        assert(optionalEffectfulFieldObjectSchema.toType_().fields(__DeprecatedArgs()).toList.flatten.headOption)(
          isSome(
            hasField[__Field, Option[List[Directive]]](
              "directives",
              _.directives.map(_.filter(_.name == "semanticNonNull")).filter(_.nonEmpty),
              isNone
            )
          )
        )
      }
    )

  case class EffectfulFieldObject(q: Task[Int], @GQLNonNullable qAnnotated: Task[Int])
  case class OptionalEffectfulFieldObject(q: Task[Option[String]], @GQLNonNullable qAnnotated: Task[Option[String]])

  implicit val effectfulFieldObjectSchema: Schema[Any, EffectfulFieldObject]                 =
    SemanticNonNullSchema.gen[Any, EffectfulFieldObject]
  implicit val optionalEffectfulFieldObjectSchema: Schema[Any, OptionalEffectfulFieldObject] =
    SemanticNonNullSchema.gen[Any, OptionalEffectfulFieldObject]
}
