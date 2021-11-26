package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.Value.IntValue
import caliban.schema.Annotations.GQLDirective
import caliban.wrappers.CostEstimation.CostDirective
import zio.{ Ref, UIO }
import zio.test.{ assertTrue, DefaultRunnableSpec, ZSpec }

object CostEstimationSpec extends DefaultRunnableSpec {
  case class WithArgs(limit: Int)
  case class E(value: Int)
  @GQLDirective(CostDirective(10))
  case class D(e: E, @GQLDirective(CostDirective(1, List("limit"))) f: List[String])
  case class Test(
    a: Int,
    @GQLDirective(CostDirective(100)) b: UIO[Int],
    @GQLDirective(CostDirective(5, multipliers = List("limit"))) c: WithArgs => UIO[List[Int]],
    @GQLDirective(CostDirective(2)) d: D
  )

  val api = graphQL(
    RootResolver(
      Test(
        1,
        UIO(2),
        args => UIO(List.fill(args.limit)(42)),
        D(E(99), List("Hello", "There"))
      )
    )
  )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("Cost Estimation")(
      testM("Rejects queries that are too expensive") {
        val wrapper = CostEstimation.maxComplexity(99)(CostEstimation.costDirective)
        for {
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("""{ b }""")
          response    <- interpreter.execute(query)
        } yield assertTrue(response.errors == List(ValidationError("Query costs too much: 101.0. Max cost: 99.", "")))
      },
      testM("Allow queries below the limit") {
        val wrapper = CostEstimation.maxComplexity(200)(CostEstimation.costDirective)
        for {
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("{ a b }")
          response    <- interpreter.execute(query)
        } yield assertTrue(response.errors.isEmpty)
      },
      testM("Applies multipliers when computing cost") {
        val wrapper = CostEstimation.maxComplexity(100)(CostEstimation.costDirective)
        for {
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("{ c(limit: 50) }")
          response    <- interpreter.execute(query)
        } yield assertTrue(response.errors.nonEmpty)
      },
      testM("Applies multipliers from variables") {
        val wrapper = CostEstimation.maxComplexity(100)(CostEstimation.costDirective)
        for {
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("""query test($limit: Int!) { b c(limit: $limit) }""")
          response    <- interpreter.execute(query, variables = Map("limit" -> IntValue(25)))
        } yield assertTrue(response.errors == List(ValidationError("Query costs too much: 226.0. Max cost: 100.", "")))
      },
      testM("Max cost with fragments") {
        val wrapper = CostEstimation.maxComplexity(5)(CostEstimation.costDirective)
        for {
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("""
               query test { # cost: 1
                 d { # cost: 2
                   ...f
                 }
               }
               
               fragment f on D { # type cost: 10
                 e { value } # cost: 1 + 1
                 f # cost: 1
               }
              """)
          response    <- interpreter.execute(query)
        } yield assertTrue(response.errors == List(ValidationError("Query costs too much: 16.0. Max cost: 5.", "")))
      },
      testM("Max cost with an effect") {
        for {
          ref         <- Ref.make(0.0)
          count       <- Ref.make(0)
          wrapper      = CostEstimation.maxComplexityZIO(100)(_ => ref.updateAndGet(_ + 10.0))
          interpreter <- (api @@ wrapper).interpreter
          query        = gqldoc("{ a }")
          response    <- interpreter.execute(query).repeatUntilM(resp => count.update(_ + 1).as(resp.errors.nonEmpty))
          c           <- count.get
        } yield assertTrue(
          response.errors == List(ValidationError("Query costs too much: 110.0. Max cost: 100.0.", ""))
        ) &&
          assertTrue(c == 11)
      }
    )
}
