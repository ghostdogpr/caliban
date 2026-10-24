package caliban.gateway.hive

import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.{ Gateway, Subgraph }
import caliban.hive.{ HiveConfig, HiveUsage, UsageEndpoint }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import zio._
import zio.Config.Secret
import zio.http.Client
import zio.test._

object GatewayHiveSpec extends ZIOSpecDefault {

  private def entries(report: ResponseValue): List[ResponseValue] =
    field(report, "map") match {
      case Some(ObjectValue(fields)) => fields.map(_._2)
      case _                         => Nil
    }

  def spec = suite("GatewayHive")(
    test("reports operations with the coordinates of the composed schema") {
      for {
        remote         <- stub(okResponse)
        endpoint       <- UsageEndpoint.start()
        (url, received) = endpoint
        config          = HiveConfig(Secret("hive-token"), "org/project/target", url, flushInterval = 1.hour)
        gateway         = Gateway.compose(Subgraph.graphql("products", remote.endpoint, valueInputSchema))
        response       <-
          ZIO
            .scoped(
              (gateway @@ GatewayHive.hooks).interpreter.flatMap(_.execute("""query V { value(input: "secret") }"""))
            )
            .provideSomeLayer[Scope with Client](HiveUsage.layer(config))
        sent           <- received.bodies.get
        reports         = sent.toList.map(readFromString[ResponseValue](_))
        reported        = reports.flatMap(entries)
      } yield assertTrue(
        response.errors.isEmpty,
        reported.flatMap(field(_, "operation")) == List(StringValue("query V{value(input:\"\")}")),
        reported.flatMap(field(_, "fields")) ==
          List(
            ListValue(
              List("Query.value", "Query.value.input", "Query.value.input!", "String").sorted.map(StringValue(_))
            )
          ),
        !sent.exists(_.contains("secret"))
      )
    },
    test("does not report an operation that fails validation") {
      for {
        remote         <- stub(okResponse)
        endpoint       <- UsageEndpoint.start()
        (url, received) = endpoint
        config          = HiveConfig(Secret("hive-token"), "org/project/target", url, flushInterval = 1.hour)
        gateway         = Gateway.compose(Subgraph.graphql("products", remote.endpoint, valueInputSchema))
        response       <- ZIO
                            .scoped((gateway @@ GatewayHive.hooks).interpreter.flatMap(_.execute("{ missing }")))
                            .provideSomeLayer[Scope with Client](HiveUsage.layer(config))
        sent           <- received.bodies.get
      } yield assertTrue(response.errors.nonEmpty, sent.isEmpty)
    }
  ).provideSomeShared[Scope](testServer, stubIds, Client.default) @@ TestAspect.sequential @@ TestAspect.withLiveClock
}
