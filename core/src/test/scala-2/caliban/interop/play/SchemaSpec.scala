package caliban.interop.play

import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import caliban.schema.Schema
import play.api.libs.json.JsValue
import zio.test.Assertion._
import zio.test._

object SchemaSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Play SchemaSpec")(
      test("field with Json object [play]") {
        import caliban.interop.play.json._
        case class Queries(to: JsValue, from: JsValue => Unit)

        assert(introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_.`type`()))(
          isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
        )
      }
    )

  def introspect[Q](implicit schema: Schema[Any, Q]): __Type = schema.toType_()
}
