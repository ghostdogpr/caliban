package caliban.schema

import caliban.introspection.adt.{ __DeprecatedArgs, __Type, __TypeKind }
import caliban.schema.SchemaSpecUtils._
import zio.test.Assertion._
import zio.test._
import zio.{ Task, UIO }

object SchemaSpec
    extends DefaultRunnableSpec(
      suite("SchemaSpec")(
        test("effectful field") {
          assert(
            introspect[EffectfulFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()),
            isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.SCALAR)))
          )
        },
        test("infallible effectful field") {
          assert(
            introspect[InfallibleFieldSchema].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()),
            isSome(hasField[__Type, __TypeKind]("kind", _.kind, equalTo(__TypeKind.NON_NULL)))
          )
        }
      )
    )

object SchemaSpecUtils {
  case class EffectfulFieldSchema(q: Task[Int])
  case class InfallibleFieldSchema(q: UIO[Int])

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType(false)
}
