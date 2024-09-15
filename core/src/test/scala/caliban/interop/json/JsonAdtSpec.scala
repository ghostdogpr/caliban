package caliban.interop.json

import caliban.schema.PureStep
import caliban.{ InputValue, ResponseValue, Value }
import zio.test._

object JsonAdtSpec extends ZIOSpecDefault {

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

  override def spec =
    suite("JsonAdtSpec")(
      suite("circe") {
        import caliban.interop.circe.json
        import io.circe._

        val jsonV = Json.obj(
          "hello"     -> Json.fromString("world"),
          "answer"    -> Json.fromInt(42),
          "isAwesome" -> Json.fromBoolean(true),
          "name"      -> Json.Null,
          "arr"       -> Json.fromValues(List(Json.fromString("first"), Json.fromInt(2))),
          "obj"       -> Json.obj("key" -> Json.fromString("value"))
        )
        List(
          test("Schema") {
            val step = json.jsonSchema.resolve(jsonV)
            assertTrue(step == PureStep(output))
          },
          test("ArgBuilder") {
            json.jsonArgBuilder.build(input).map(resp => assertTrue(resp == jsonV))
          }
        )
      },
      suite("play-json") {
        import caliban.interop.play.json
        import play.api.libs.json._

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
            val step = json.jsonSchema.resolve(jsonV)
            assertTrue(step == PureStep(output))
          },
          test("ArgBuilder") {
            json.jsonArgBuilder.build(input).map(resp => assertTrue(resp == jsonV))
          }
        )
      },
      suite("zio-json") {
        import caliban.interop.zio.json
        import zio.json.ast._

        val jsonV = Json.Obj(
          "hello"     -> Json.Str("world"),
          "answer"    -> Json.Num(42),
          "isAwesome" -> Json.Bool(true),
          "name"      -> Json.Null,
          "arr"       -> Json.Arr(Json.Str("first"), Json.Num(2)),
          "obj"       -> Json.Obj("key" -> Json.Str("value"))
        )

        List(
          test("Schema") {
            val step = json.jsonSchema.resolve(jsonV)
            assertTrue(step == PureStep(output))
          },
          test("ArgBuilder") {
            json.jsonArgBuilder.build(input).map(resp => assertTrue(resp == jsonV))
          }
        )
      }
    )
}
