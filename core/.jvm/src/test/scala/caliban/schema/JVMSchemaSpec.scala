package caliban.schema

import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import spray.json.JsValue
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object JVMSchemaSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SchemaSpec")(
      test("field with Json object") {
        import caliban.interop.spray.json._

        case class Queries(to: JsValue, from: JsValue => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      }
    )

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType()
}
