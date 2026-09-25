package caliban.gateway

import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.execution.ExecutionRequest
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal._
import caliban.validation.Validator
import caliban.{ CalibanError, Configurator, GraphQLRequest, InputValue }
import zio._
import zio.http._
import zio.test._

object RuntimeBoundsSpec extends ZIOSpecDefault {

  private val request = GraphQLRequest(query = Some("query Value { value }"), operationName = Some("Value"))

  def spec = suite("RuntimeBoundsSpec")(
    suite("operation cache")(
      test("single-flights concurrent misses") {
        val requests = 32
        for {
          cache        <- OperationCache.make[String, String, Int, Any](64, PhaseHooks.empty)
          ready        <- Ref.make(0)
          start        <- Promise.make[Nothing, Unit]
          computing    <- Promise.make[Nothing, Unit]
          release      <- Promise.make[Nothing, Unit]
          computations <- Ref.make(0)
          fibers       <- ZIO.foreach(1 to requests)(_ =>
                            (ready.update(_ + 1) *> start.await *>
                              cache.getOrCompute("same")(
                                computations.update(_ + 1) *>
                                  computing.succeed(()).unit *>
                                  release.await.as(Weighted(1, 8))
                              )).fork
                          )
          _            <- ready.get.repeatUntil(_ == requests)
          _            <- start.succeed(())
          _            <- computing.await
          _            <- ZIO.yieldNow.repeatN(requests)
          _            <- release.succeed(())
          values       <- ZIO.foreach(fibers)(_.join)
          runs         <- computations.get
        } yield assertTrue(
          values.forall(_ == 1),
          runs == 1
        )
      },
      test("evicts entries by total weight") {
        for {
          cache <- OperationCache.make[String, String, Int, Any](5, PhaseHooks.empty)
          runs  <- Ref.make(0)
          _     <- cache.getOrCompute("first")(runs.update(_ + 1).as(Weighted(1, 3)))
          _     <- cache.getOrCompute("second")(runs.update(_ + 1).as(Weighted(2, 3)))
          first <- cache.getOrCompute("first")(runs.update(_ + 1).as(Weighted(1, 3)))
          count <- runs.get
        } yield assertTrue(
          first == 1,
          count == 3
        )
      },
      test("allows an interrupted waiter to leave without cancelling the shared computation") {
        for {
          recorded       <- recordEvents
          (events, hooks) = recorded
          cache          <- OperationCache.make[String, String, Int, Any](32, hooks)
          computing      <- Promise.make[Nothing, Unit]
          release        <- Promise.make[Nothing, Unit]
          computations   <- Ref.make(0)
          leader         <- cache
                              .getOrCompute("same")(
                                computations.update(_ + 1) *>
                                  computing.succeed(()).unit *>
                                  release.await.as(Weighted(1, 4))
                              )
                              .fork
          _              <- computing.await
          waiter         <- cache.getOrCompute("same")(ZIO.dieMessage("waiter computed")).fork
          _              <- events.get.repeatUntil(_.contains(PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Wait)))
          waiterExit     <- waiter.interrupt
          _              <- release.succeed(())
          leaderValue    <- leader.join
          cached         <- cache.getOrCompute("same")(ZIO.dieMessage("cache missed"))
          runs           <- computations.get
        } yield assertTrue(waiterExit.isInterrupted, leaderValue == 1, cached == 1, runs == 1)
      },
      test("cleans up an in-flight entry when the miss hook interrupts") {
        for {
          interrupt <- Ref.make(true)
          hooks      = PhaseHooks.cacheAccess(PhaseHandler.incomingDiscard {
                         case PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Miss) =>
                           interrupt.getAndSet(false).flatMap(if (_) ZIO.interrupt else ZIO.unit)
                         case _                                                         => ZIO.unit
                       })
          cache     <- OperationCache.make[String, String, Int, Any](32, hooks)
          first     <- cache.getOrCompute("same")(ZIO.succeed(Weighted(1, 4))).exit
          second    <- cache.getOrCompute("same")(ZIO.succeed(Weighted(2, 4)))
        } yield assertTrue(
          first.isInterrupted,
          second == 2
        )
      },
      test("retries a waiter when the compute leader is interrupted before computation starts") {
        for {
          firstMiss <- Ref.make(true)
          entered   <- Promise.make[Nothing, Unit]
          joined    <- Promise.make[Nothing, Unit]
          hooks      = PhaseHooks.cacheAccess(PhaseHandler.incomingDiscard {
                         case PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Miss) =>
                           firstMiss.getAndSet(false).flatMap {
                             case true  => entered.succeed(()).unit *> ZIO.never
                             case false => ZIO.unit
                           }
                         case PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Wait) =>
                           joined.succeed(()).unit
                         case _                                                         => ZIO.unit
                       })

          cache      <- OperationCache.make[String, String, Int, Any](32, hooks)
          leader     <- cache.getOrCompute("same")(ZIO.succeed(Weighted(1, 4))).fork
          _          <- entered.await
          waiter     <- cache.getOrCompute("same")(ZIO.succeed(Weighted(2, 4))).fork
          _          <- joined.await
          leaderExit <- leader.interrupt
          value      <- waiter.join
        } yield assertTrue(leaderExit.isInterrupted, value == 2)
      }
    ),
    suite("operation preparation")(
      test("reuses variable-free operations independently of other operations and authorization") {
        val multipleOperations = request.copy(query =
          Some(
            "query Value { value } query Other($include: Boolean!) { value @include(if: $include) }"
          )
        )
        for {
          recorded       <- recordEvents
          (events, hooks) = recorded
          executions     <- Ref.make(Vector.empty[ExecutionRequest])
          stableRemote   <- stub(okResponse)
          stable         <-
            Gateway
              .compose(Subgraph.graphql("stable", stableRemote.endpoint, valueInputSchema))
              .withPhaseHooks(
                PhaseHooks.authorization[Any](operation => executions.update(_ :+ operation.executionRequest))
              )
              .withPhaseHooks(hooks)
              .interpreter
          first          <- stable.executeRequest(multipleOperations)
          second         <- stable.executeRequest(multipleOperations)
          authorized     <- executions.get
          observed       <- events.get
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
          observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Hit)) == 1,
          authorized.size == 2,
          authorized.head eq authorized.last
        )
      },
      test("caches custom validations and isolates different validation lists and gateway instances") {
        val allow: Validator.QueryValidation = _ => Right(())
        val deny: Validator.QueryValidation  = _ => Left(CalibanError.ValidationError("Custom validation rejected.", ""))
        val allowed                          = Validator.AllValidations :+ allow
        val denied                           = Validator.AllValidations :+ deny

        def execute(runtime: GatewayInterpreter[Any], validations: List[Validator.QueryValidation]) =
          ZIO.scoped(Configurator.setValidations(validations) *> runtime.executeRequest(request))

        for {
          recorded       <- recordEvents
          (events, hooks) = recorded
          gateway         = localGateway(ZIO.succeed("ok")).withPhaseHooks(hooks)
          runtime        <- gateway.interpreter
          first          <- execute(runtime, allowed)
          second         <- execute(runtime, allowed.map(identity))
          rejected       <- execute(runtime, denied)
          restored       <- execute(runtime, allowed)
          other          <- gateway.interpreter
          otherRejected  <- execute(other, denied)
          observed       <- events.get
          accesses        = observed.collect { case PhaseHooks.Event.CacheAccess(result) => result }
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
          restored.errors.isEmpty,
          rejected.errors.map(_.msg) == List("Custom validation rejected."),
          otherRejected.errors.map(_.msg) == List("Custom validation rejected."),
          accesses == Vector(
            PhaseHooks.CacheResult.Miss,
            PhaseHooks.CacheResult.Hit,
            PhaseHooks.CacheResult.Miss,
            PhaseHooks.CacheResult.Hit,
            PhaseHooks.CacheResult.Miss
          )
        )
      },
      test("isolates cached preparations by the Caliban introspection setting") {
        val schemaIntrospection = GraphQLRequest(query = Some("{ __schema { queryType { name } } }"))
        val typeIntrospection   = GraphQLRequest(query = Some("{ __type(name: \"Query\") { name } }"))
        val disabled            = Configurator.ExecutionConfiguration(enableIntrospection = false)
        val enabled             = Configurator.ExecutionConfiguration(enableIntrospection = true)

        for {
          remote         <- stub(okResponse)
          runtime        <- remoteGateway(remote.endpoint).interpreter
          enabledMiss    <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          enabledHit     <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          disabledSchema <- Configurator.locally(disabled)(runtime.executeRequest(schemaIntrospection))
          disabledType   <- Configurator.locally(disabled)(runtime.executeRequest(typeIntrospection))
          reenabled      <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          sent           <- remote.requests.get
          disabledResults = disabledSchema :: disabledType :: Nil
        } yield assertTrue(
          enabledMiss.errors.isEmpty,
          enabledHit.errors.isEmpty,
          reenabled.errors.isEmpty,
          disabledResults.forall(_.errors.map(_.msg) == List("Introspection is disabled")),
          sent.isEmpty
        )
      },
      test("reuses one plan across variable values and binds each request separately") {
        val variableSchema = "type Query { value(input: String!): String }"
        val variableQuery  = "query Value($input: String!) { value(input: $input) __typename }"

        def valueRequest(input: String): GraphQLRequest =
          GraphQLRequest(
            query = Some(variableQuery),
            operationName = Some("Value"),
            variables = Some(Map("input" -> StringValue(input)))
          )

        for {
          recorded       <- recordEvents
          (events, hooks) = recorded
          remote         <- stub(okResponse, okResponse)
          runtime        <- remoteGateway(remote.endpoint, variableSchema).withPhaseHooks(hooks).interpreter
          first          <- runtime.executeRequest(valueRequest("first"))
          second         <- runtime.executeRequest(valueRequest("second"))
          sent           <- remote.requests.get
          observed       <- events.get
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
          observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Miss)) == 1,
          observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Hit)) == 1,
          sent.flatMap(_.query) == Vector(
            "query Value{value(input:\"first\")}",
            "query Value{value(input:\"second\")}"
          )
        )
      },
      test("replans variable-conditioned selections without repeating static preparation") {
        val conditionalSchema = "type Query { firstValue: String conditionalValue: String }"
        val conditionalQuery  =
          "query Values($include: Boolean!) { firstValue conditionalValue @include(if: $include) __typename }"
        val conditionalResult = """{"data":{"firstValue":"first","conditionalValue":"included"}}"""

        def valuesRequest(include: Boolean): GraphQLRequest =
          GraphQLRequest(
            query = Some(conditionalQuery),
            operationName = Some("Values"),
            variables = Some(Map("include" -> BooleanValue(include)))
          )

        for {
          recorded       <- recordEvents
          (events, hooks) = recorded
          remote         <- stub(conditionalResult, conditionalResult)
          runtime        <- remoteGateway(remote.endpoint, conditionalSchema).withPhaseHooks(hooks).interpreter
          _              <- runtime.executeRequest(valuesRequest(false))
          _              <- runtime.executeRequest(valuesRequest(true))
          sent           <- remote.requests.get
          observed       <- events.get
        } yield assertTrue(
          observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Miss)) == 1,
          observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Hit)) == 1,
          !sent.headOption.flatMap(_.query).exists(_.contains("conditionalValue")),
          sent.drop(1).headOption.flatMap(_.query).exists(_.contains("conditionalValue"))
        )
      },
      test("validates OneOf variables on cached misses and hits") {
        val oneOfSchema =
          "input Choice @oneOf { first: String second: String } type Query { choose(input: Choice!): String }"
        val oneOfQuery  = "query Choose($input: Choice!) { choose(input: $input) }"
        for {
          remote         <- stub(okResponse)
          runtime        <- remoteGateway(remote.endpoint, oneOfSchema).interpreter
          multipleFields <- runtime.executeRequest(
                              GraphQLRequest(
                                query = Some(oneOfQuery),
                                operationName = Some("Choose"),
                                variables = Some(
                                  Map(
                                    "input" -> InputValue.ObjectValue(
                                      Map("first" -> StringValue("first"), "second" -> StringValue("second"))
                                    )
                                  )
                                )
                              )
                            )
          nullField      <- runtime.executeRequest(
                              GraphQLRequest(
                                query = Some(oneOfQuery),
                                operationName = Some("Choose"),
                                variables = Some(Map("input" -> InputValue.ObjectValue(Map("first" -> NullValue))))
                              )
                            )
          sent           <- remote.requests.get
        } yield assertTrue(
          multipleFields.errors.nonEmpty,
          nullField.errors.nonEmpty,
          sent.isEmpty
        )
      },
      test("rejects invalid gateway bounds before constructing a runtime") {
        for {
          exit <- localGateway(ZIO.succeed("ok"))
                    .withConfig(
                      _.withMaxPlanningCandidates(0)
                        .withMaxPlanningExpansions(0)
                        .withPlanningTimeout(Duration.Infinity)
                    )
                    .interpreter
                    .exit
        } yield assertTrue(
          buildDiagnostics(exit) == List(
            "Gateway maxPlanningCandidates must be positive.",
            "Gateway maxPlanningExpansions must be positive.",
            "Gateway planning timeout must be finite and positive."
          )
        )
      }
    ),
    suite("concurrent execution")(
      test("runs concurrent local requests") {
        val concurrency = 65
        for {
          started <- Ref.make(0)
          release <- Promise.make[Nothing, Unit]
          runtime <- localGateway(started.update(_ + 1) *> release.await.as("ok")).interpreter
          fibers  <- ZIO.foreach(1 to concurrency)(_ => runtime.execute("{ value }").fork)
          _       <- started.get.repeatUntil(_ == concurrency)
          _       <- release.succeed(())
          results <- ZIO.foreach(fibers)(_.join)
        } yield assertTrue(results.forall(_.errors.isEmpty))
      },
      test("runs local and remote sources concurrently") {
        for {
          localStarted  <- Promise.make[Nothing, Unit]
          remoteStarted <- Promise.make[Nothing, Unit]
          release       <- Promise.make[Nothing, Unit]
          remote        <- stubWith(remoteStarted.succeed(()).unit *> release.await, okResponse)
          runtime       <- Gateway
                             .compose(
                               Subgraph.graphql(
                                 "local",
                                 localValueGraph(localStarted.succeed(()).unit *> release.await.as("local"))
                               ),
                               Subgraph.graphql("remote", remote.endpoint, valueInputSchema)
                             )
                             .interpreter
          fiber         <- runtime.execute("{ localValue value }").fork
          _             <- localStarted.await.zipPar(remoteStarted.await)
          _             <- release.succeed(())
          result        <- fiber.join
        } yield assertTrue(
          field(result.data, "localValue").contains(StringValue("local")),
          field(result.data, "value").contains(StringValue("ok"))
        )
      },
      test("runs independent calls while another call retries") {
        val config = RemoteGraphQLConfig.default.withExecution(
          _.withRetries(1, Duration.Zero)
            .withInFlightQueryDeduplication(false)
        )
        for {
          calls        <- Ref.make(0)
          retryStarted <- Promise.make[Nothing, Unit]
          releaseRetry <- Promise.make[Nothing, Unit]
          remote       <- postEndpoint("runtime-bounds") { _ =>
                            calls.updateAndGet(_ + 1).flatMap {
                              case 1 => ZIO.succeed(Response.status(Status.ServiceUnavailable))
                              case 2 =>
                                retryStarted.succeed(()).unit *> releaseRetry.await.as(graphQLResponse(okResponse))
                              case _ => ZIO.succeed(graphQLResponse(okResponse))
                            }
                          }
          runtime      <- remoteGateway(remote, config = config).interpreter
          first        <- runtime.executeRequest(request).fork
          _            <- retryStarted.await
          second       <- runtime.executeRequest(request).fork
          secondResult <- second.join
          before       <- calls.get
          _            <- releaseRetry.succeed(())
          firstResult  <- first.join
          total        <- calls.get
        } yield assertTrue(
          before == 3,
          firstResult.errors.isEmpty,
          secondResult.errors.isEmpty,
          total == 3
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
