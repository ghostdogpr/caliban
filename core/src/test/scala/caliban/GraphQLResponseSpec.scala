package caliban

import caliban.CalibanError.{ ExecutionError, ParsingError, ValidationError }
import caliban.GraphQLResponseContext.Outcome
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.schema.Schema.auto._
import caliban.wrappers.Wrapper.OverallWrapper
import zio.ZIO
import zio.test._

object GraphQLResponseSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Nothing] =
    suite("GraphQLResponseSpec")(
      test("withExtension replaces an existing extension with the same key") {
        val response = GraphQLResponse(Value.NullValue, Nil)
          .withExtension("tracing", StringValue("v1"))
          .withExtension("cache", StringValue("cache"))
          .withExtension("tracing", StringValue("v2"))

        assertTrue(
          response.extensions == Some(
            ObjectValue(
              List(
                "tracing" -> StringValue("v2"),
                "cache"   -> StringValue("cache")
              )
            )
          )
        )
      },
      test("classifies only request errors from an overall wrapper as request errors") {
        final case class Query(value: String)

        def shortCircuit(error: CalibanError): OverallWrapper[Any] =
          new OverallWrapper[Any] {
            def wrap[R1 <: Any](
              _process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
            ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
              _ => ZIO.succeed(GraphQLResponse(NullValue, List(error)))
          }

        val cases = List[(CalibanError, Outcome)](
          ParsingError("bad syntax")        -> Outcome.RequestError,
          ValidationError("bad query", "")  -> Outcome.RequestError,
          ExecutionError("resolver failed") -> Outcome.Executed
        )

        ZIO
          .foreach(cases) { case (error, expected) =>
            for {
              interpreter <- (graphQL(RootResolver(Query("ok"))) @@ shortCircuit(error)).interpreter.orDie
              outcome     <- interpreter.executeRequestWith(GraphQLRequest(query = Some("{ value }")))(_.outcome)
            } yield outcome -> expected
          }
          .map(outcomes => assertTrue(outcomes.forall { case (actual, expected) => actual == expected }))
      }
    )
}
