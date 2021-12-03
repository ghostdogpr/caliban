package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.ResponseValue.ObjectValue
import caliban.RootResolver
import caliban.Value.{ FloatValue, IntValue }
import caliban.wrappers.CostEstimation.GQLCost
import zio.Console.print
import zio.test._
import zio.{ Ref, UIO }

object CostEstimationSpec extends DefaultRunnableSpec {
  case class WithArgs(limit: Int)
  case class E(value: Int)
  @GQLCost(10)
  case class D(e: E, @GQLCost(1, List("limit")) f: List[String])
  case class Test(
    a: Int,
    @GQLCost(100) b: UIO[Int],
    @GQLCost(5, multipliers = List("limit")) c: WithArgs => UIO[List[Int]],
    @GQLCost(2) d: D
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

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Cost Estimation")(
      suite("maxCost")(
        test("Rejects queries that are too expensive") {
          val wrapper = CostEstimation.maxCost(99)(CostEstimation.costDirective)
          for {
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("""{ b }""")
            response    <- interpreter.execute(query)
          } yield assertTrue(
            response.errors == List(ValidationError("Query costs too much: 101.0. Max cost: 99.0.", ""))
          )
        },
        test("Allow queries below the limit") {
          val wrapper = CostEstimation.maxCost(200)(CostEstimation.costDirective)
          for {
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("{ a b }")
            response    <- interpreter.execute(query)
          } yield assertTrue(response.errors.isEmpty)
        },
        test("Applies multipliers when computing cost") {
          val wrapper = CostEstimation.maxCost(100)(CostEstimation.costDirective)
          for {
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("{ c(limit: 50) }")
            response    <- interpreter.execute(query)
          } yield assertTrue(response.errors.nonEmpty)
        },
        test("Applies multipliers from variables") {
          val wrapper = CostEstimation.maxCost(100)(CostEstimation.costDirective)
          for {
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("""query test($limit: Int!) { b c(limit: $limit) }""")
            response    <- interpreter.execute(query, variables = Map("limit" -> IntValue(25)))
          } yield assertTrue(
            response.errors == List(ValidationError("Query costs too much: 226.0. Max cost: 100.0.", ""))
          )
        },
        test("Max cost with fragments") {
          val wrapper = CostEstimation.maxCost(5)(CostEstimation.costDirective)
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
          } yield assertTrue(response.errors == List(ValidationError("Query costs too much: 6.0. Max cost: 5.0.", "")))
        },
        test("Max cost with an effect") {
          for {
            ref         <- Ref.make(0.0)
            count       <- Ref.make(0)
            wrapper      = CostEstimation.maxCostZIO(500)(_ => ref.updateAndGet(_ + 10.0))
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("{ a d { e { value } f } }")
            response    <- interpreter.execute(query).repeatUntilZIO(resp => count.update(_ + 1).as(resp.errors.nonEmpty))
            c           <- count.get
          } yield assertTrue(
            response.errors == List(ValidationError("Query costs too much: 570.0. Max cost: 500.0.", ""))
          ) &&
            assertTrue(c == 2)
        }
      ),
      suite("queryCost")(
        test("includes the cost as an extension") {
          val wrapper = CostEstimation.queryCost(CostEstimation.costDirective)
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
          } yield assertTrue(response.extensions.get == ObjectValue(List("queryCost" -> FloatValue(6.0))))
        },
        test("execute arbitrary side-effect with query cost") {
          val wrapper =
            CostEstimation.queryCostWith(CostEstimation.costDirective)(total => print(s"Query cost $total").orDie)
          for {
            interpreter <- (api @@ wrapper).interpreter
            query        = gqldoc("""
               query test { # cost: 1
                 d { # cost: 2
                   ...f
                 }
               }
               
               fragment f on D { # type cost: 10 overridden by the field
                 e { value } # cost: 1 + 1
                 f # cost: 1
               }
              """)
            _           <- interpreter.execute(query)
            output      <- TestConsole.output
          } yield assertTrue(output.head == "Query cost 6.0")
        }
      )
    )
}
