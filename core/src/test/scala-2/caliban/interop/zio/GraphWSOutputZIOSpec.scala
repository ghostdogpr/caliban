//package caliban.interop.zio
//
//import caliban.{ GraphQLWSOutput, ResponseValue, Value }
//import zio.json._
//import zio.test.Assertion._
//import zio.test._
//
//object GraphWSOutputZIOSpec extends DefaultRunnableSpec {
//  override def spec: ZSpec[TestEnvironment, Any] =
//    suite("GraphWSOutputZIOSpec")(
//      test("can be parsed from JSON by zio-json") {
//        val request =
//          """{"id":"id","type":"some type","payload":{"field":"yo"}}"""
//
//        val res = request.fromJson[GraphQLWSOutput]
//        assert(res)(
//          isRight(
//            equalTo(
//              GraphQLWSOutput(
//                `type` = "some type",
//                id = Some("id"),
//                payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
//              )
//            )
//          )
//        )
//      },
//      test("can encode to JSON by zio-json") {
//        val res = GraphQLWSOutput(
//          `type` = "some type",
//          id = Some("id"),
//          payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
//        )
//
//        assertTrue(res.toJson == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
//      }
//    )
//}
