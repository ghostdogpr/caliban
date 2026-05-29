package caliban.interop.play

import caliban.Macros.gqldoc
import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import caliban.schema.{ PureStep, Schema, SchemaSpec }
import caliban.schema.Schema.auto._
import caliban.{ graphQL, InputValue, ResponseValue, RootResolver, Value }
import play.api.libs.json._
import zio.test.Assertion._
import zio.test._

object PlayInteropSpec extends ZIOSpecDefault {

  private val input = InputValue.ObjectValue(
    Map(
      "hello"     -> Value.StringValue("world"),
      "answer"    -> Value.IntValue(42),
      "isAwesome" -> Value.BooleanValue(true),
      "name"      -> Value.NullValue,
      "arr"       -> InputValue.ListValue(List(Value.StringValue("first"), Value.IntValue(2))),
      "obj"       -> InputValue.ObjectValue(Map("key" -> Value.StringValue("value")))
    )
  )

  private val output = ResponseValue.ObjectValue(
    List(
      "hello"     -> Value.StringValue("world"),
      "answer"    -> Value.IntValue(42),
      "isAwesome" -> Value.BooleanValue(true),
      "name"      -> Value.NullValue,
      "arr"       -> ResponseValue.ListValue(List(Value.StringValue("first"), Value.IntValue(2))),
      "obj"       -> ResponseValue.ObjectValue(List("key" -> Value.StringValue("value")))
    )
  )

  override def spec = suite("play-json interop")(
    suite("JsonAdt")({
      val jsonV = Json.obj(
        "hello"     -> JsString("world"),
        "answer"    -> JsNumber(42),
        "isAwesome" -> JsBoolean(true),
        "name"      -> JsNull,
        "arr"       -> JsArray(Seq(JsString("first"), JsNumber(2))),
        "obj"       -> Json.obj("key" -> JsString("value"))
      )
      List(
        test("Schema") {
          val step = caliban.interop.play.json.jsonSchema.resolve(jsonV)
          assertTrue(step == PureStep(output))
        },
        test("ArgBuilder") {
          caliban.interop.play.json.jsonArgBuilder.build(input).map(resp => assertTrue(resp == jsonV))
        }
      )
    }),
    test("Schema field with Json object [play]") {
      import caliban.interop.play.json._
      case class Queries(to: JsValue, from: JsValue => Unit)

      assert(SchemaSpec.introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.headOption.map(_._type))(
        isSome(hasField[__Type, String]("to", _.ofType.flatMap(_.name).get, equalTo("Json")))
      )
    },
    test("Execution: Play Json scalar") {
      import caliban.interop.play.json._
      case class Queries(test: JsValue)

      val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", JsNumber(333)))))).interpreter
      val query       = gqldoc("""
           {
             test
           }""")

      interpreter.flatMap(_.execute(query)).map { response =>
        assertTrue(response.data.toString == """{"test":{"a":333}}""")
      }
    }
  )
}
