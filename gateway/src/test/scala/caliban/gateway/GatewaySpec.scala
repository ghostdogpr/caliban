package caliban.gateway

import caliban.Value.NullValue
import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest, GraphQLResponse }
import zio._
import zio.test._

object GatewaySpec extends ZIOSpecDefault {

  private trait Service

  private object TestRuntime extends GatewayRuntime[Any] {
    def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] = ZIO.unit

    def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[Any, GraphQLResponse[CalibanError]] =
      ZIO.succeed(GraphQLResponse(NullValue, Nil))
  }

  private val testGateway: Gateway[Any] = new Gateway[Any](_ => ZIO.succeed(TestRuntime))

  def spec = suite("GatewaySpec")(
    test("build produces a scoped runtime") {
      ZIO.scoped[Any](testGateway.build.map(runtime => assertTrue(runtime == TestRuntime)))
    },
    test("a runtime is usable wherever a Caliban interpreter is expected") {
      val interpreter: GraphQLInterpreter[Any, CalibanError] = TestRuntime
      interpreter.execute("{ __typename }").map(response => assertTrue(response.errors.isEmpty))
    },
    test("the description and the runtime are contravariant in their environment") {
      val gateway: Gateway[Service]        = testGateway
      val runtime: GatewayRuntime[Service] = TestRuntime
      assertTrue(gateway == testGateway, runtime == TestRuntime)
    }
  )
}
