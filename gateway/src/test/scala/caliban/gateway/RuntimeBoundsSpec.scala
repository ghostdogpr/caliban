package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.Allow
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal._
import caliban.{ Configurator, GraphQLRequest, InputValue }
import sttp.model.Uri
import zio._
import zio.http.{ Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.metrics.Metric
import zio.test._

object RuntimeBoundsSpec extends ZIOSpecDefault {

  private val response = """{"data":{"value":"ok"}}"""
  private val schema   = "type Query { value: String }"
  private val request  = GraphQLRequest(query = Some("query Value { value }"), operationName = Some("Value"))

  private def endpoint(handler: Request => UIO[Response]): ZIO[Server with Ref[Int], Nothing, Uri] =
    postEndpoint("runtime-bounds")(handler)

  private def graphQLResponse(value: String): Response =
    Response(
      Status.Ok,
      Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
      Body.fromString(value)
    )

  def spec = suite("RuntimeBoundsSpec")(
    suite("operation cache")(
      test("single-flights concurrent misses") {
        val requests = 32
        for {
          cache        <- OperationCache.make[String, String, Int, Any](64, GatewayWrapper.empty)
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
          status       <- cache.status
        } yield assertTrue(
          values.forall(_ == 1),
          runs == 1,
          status.entries == 1,
          status.inFlight == 0
        )
      },
      test("evicts entries by total weight") {
        for {
          cache  <- OperationCache.make[String, String, Int, Any](5, GatewayWrapper.empty)
          runs   <- Ref.make(0)
          _      <- cache.getOrCompute("first")(runs.update(_ + 1).as(Weighted(1, 3)))
          _      <- cache.getOrCompute("second")(runs.update(_ + 1).as(Weighted(2, 3)))
          first  <- cache.getOrCompute("first")(runs.update(_ + 1).as(Weighted(1, 3)))
          count  <- runs.get
          status <- cache.status
        } yield assertTrue(
          first == 1,
          count == 3,
          status.weight <= status.maxWeight,
          status.evictions == 2
        )
      },
      test("allows an interrupted waiter to leave without cancelling the shared computation") {
        for {
          cache        <- OperationCache.make[String, String, Int, Any](32, GatewayWrapper.empty)
          computing    <- Promise.make[Nothing, Unit]
          release      <- Promise.make[Nothing, Unit]
          computations <- Ref.make(0)
          leader       <- cache
                            .getOrCompute("same")(
                              computations.update(_ + 1) *>
                                computing.succeed(()).unit *>
                                release.await.as(Weighted(1, 4))
                            )
                            .fork
          _            <- computing.await
          waiter       <- cache.getOrCompute("same")(ZIO.dieMessage("waiter computed")).fork
          _            <- cache.status.repeatUntil(_.misses >= 2)
          waiterExit   <- waiter.interrupt
          _            <- release.succeed(())
          leaderValue  <- leader.join
          cached       <- cache.getOrCompute("same")(ZIO.dieMessage("cache missed"))
          runs         <- computations.get
        } yield assertTrue(waiterExit.isInterrupted, leaderValue == 1, cached == 1, runs == 1)
      },
      test("cleans up an in-flight entry when the miss wrapper interrupts") {
        for {
          interrupt   <- Ref.make(true)
          wrapper      = new GatewayWrapper[Any] {
                           def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
                             result: Exit[E, A] => GatewayWrapper.Result
                           )(implicit trace: Trace): ZIO[R0, E, A] =
                             event match {
                               case GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss) =>
                                 interrupt.getAndSet(false).flatMap(if (_) ZIO.interrupt else effect)
                               case _                                                                 => effect
                             }
                         }
          cache       <- OperationCache.make[String, String, Int, Any](32, wrapper)
          first       <- cache.getOrCompute("same")(ZIO.succeed(Weighted(1, 4))).exit
          afterFirst  <- cache.status
          second      <- cache.getOrCompute("same")(ZIO.succeed(Weighted(2, 4)))
          afterSecond <- cache.status
        } yield assertTrue(
          first.isInterrupted,
          afterFirst.inFlight == 0,
          second == 2,
          afterSecond.inFlight == 0,
          afterSecond.entries == 1
        )
      }
    ),
    suite("operation preparation")(
      test("caches prepared plans independently of policy evaluation") {
        for {
          policyCalls  <- Ref.make(0)
          stableRemote <- stub(response)
          stable       <- Gateway
                            .compose(Subgraph.graphql("stable", stableRemote.endpoint, schema))
                            .withOperationPolicy(
                              OperationPolicy[Any](_ => policyCalls.update(_ + 1).as(Allow))
                            )
                            .interpreter
          _            <- stable.executeRequest(request)
          _            <- stable.executeRequest(request)
          stableStatus <- stable.status
          policyRuns   <- policyCalls.get
        } yield assertTrue(
          stableStatus.operationCache.entries == 1,
          stableStatus.operationCache.misses == 1,
          stableStatus.operationCache.hits == 1,
          policyRuns == 2
        )
      },
      test("isolates cached preparations by the Caliban introspection setting") {
        val schemaIntrospection = GraphQLRequest(query = Some("{ __schema { queryType { name } } }"))
        val typeIntrospection   = GraphQLRequest(query = Some("{ __type(name: \"Query\") { name } }"))
        val disabled            = Configurator.ExecutionConfiguration(enableIntrospection = false)
        val enabled             = Configurator.ExecutionConfiguration(enableIntrospection = true)

        for {
          remote         <- stub(response)
          runtime        <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema)).interpreter
          enabledMiss    <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          enabledHit     <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          disabledSchema <- Configurator.locally(disabled)(runtime.executeRequest(schemaIntrospection))
          disabledType   <- Configurator.locally(disabled)(runtime.executeRequest(typeIntrospection))
          reenabled      <- Configurator.locally(enabled)(runtime.executeRequest(schemaIntrospection))
          status         <- runtime.status
          forwarded      <- remote.requests.get
          disabledResults = disabledSchema :: disabledType :: Nil
        } yield assertTrue(
          enabledMiss.errors.isEmpty,
          enabledHit.errors.isEmpty,
          reenabled.errors.isEmpty,
          disabledResults.forall(_.errors.map(_.msg) == List("Introspection is disabled")),
          status.operationCache.entries == 1,
          status.operationCache.misses == 3,
          status.operationCache.hits == 2,
          forwarded.isEmpty
        )
      },
      test("reuses one plan across variable values and binds each request separately") {
        val variableSchema = "type Query { value(input: String!): String }"
        val variableQuery  = "query Value($input: String!) { value(input: $input) __typename }"
        for {
          remote  <- stub(response, response)
          runtime <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, variableSchema)).interpreter
          first   <- runtime.executeRequest(
                       GraphQLRequest(
                         query = Some(variableQuery),
                         operationName = Some("Value"),
                         variables = Some(Map("input" -> StringValue("first")))
                       )
                     )
          second  <- runtime.executeRequest(
                       GraphQLRequest(
                         query = Some(variableQuery),
                         operationName = Some("Value"),
                         variables = Some(Map("input" -> StringValue("second")))
                       )
                     )
          sent    <- remote.requests.get
          status  <- runtime.status
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
          sent.flatMap(_.query) == Vector(
            "query Value{value(input:\"first\")}",
            "query Value{value(input:\"second\")}"
          ),
          status.operationCache.entries == 1,
          status.operationCache.misses == 1,
          status.operationCache.hits == 1
        )
      },
      test("replans variable-conditioned selections without repeating static preparation") {
        val conditionalSchema = "type Query { firstValue: String conditionalValue: String }"
        val conditionalQuery  =
          "query Values($include: Boolean!) { firstValue conditionalValue @include(if: $include) __typename }"
        val conditionalResult = """{"data":{"firstValue":"first","conditionalValue":"included"}}"""
        for {
          remote  <- stub(conditionalResult, conditionalResult)
          runtime <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, conditionalSchema)).interpreter
          _       <- runtime.executeRequest(
                       GraphQLRequest(
                         query = Some(conditionalQuery),
                         operationName = Some("Values"),
                         variables = Some(Map("include" -> BooleanValue(false)))
                       )
                     )
          _       <- runtime.executeRequest(
                       GraphQLRequest(
                         query = Some(conditionalQuery),
                         operationName = Some("Values"),
                         variables = Some(Map("include" -> BooleanValue(true)))
                       )
                     )
          sent    <- remote.requests.get
          status  <- runtime.status
        } yield assertTrue(
          !sent.headOption.flatMap(_.query).exists(_.contains("conditionalValue")),
          sent.drop(1).headOption.flatMap(_.query).exists(_.contains("conditionalValue")),
          status.operationCache.entries == 1,
          status.operationCache.misses == 1,
          status.operationCache.hits == 1
        )
      },
      test("validates OneOf variables on cached misses and hits") {
        val oneOfSchema =
          "input Choice @oneOf { first: String second: String } type Query { choose(input: Choice!): String }"
        val oneOfQuery  = "query Choose($input: Choice!) { choose(input: $input) }"
        for {
          remote         <- stub(response)
          runtime        <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, oneOfSchema)).interpreter
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
          status         <- runtime.status
        } yield assertTrue(
          multipleFields.errors.nonEmpty,
          nullField.errors.nonEmpty,
          sent.isEmpty,
          status.operationCache.entries == 1,
          status.operationCache.misses == 1,
          status.operationCache.hits == 1
        )
      },
      test("rejects oversized, over-nested, and over-structured operations before source calls") {
        val limitsSchema = "type Query { a: String b: String c: String nested: Item } type Item { value: String }"
        for {
          remote       <- stub("""{"data":{"a":"a","b":"b","c":"c"}}""")
          textRuntime  <- Gateway
                            .compose(Subgraph.graphql("text", remote.endpoint, limitsSchema))
                            .withConfig(_.withMaxOperationTextBytes(4))
                            .interpreter
          text         <- textRuntime.execute("{ a }")
          depthRuntime <- Gateway
                            .compose(Subgraph.graphql("depth", remote.endpoint, limitsSchema))
                            .withConfig(_.withMaxOperationNesting(1))
                            .interpreter
          depth        <- depthRuntime.execute("{ nested { value } }")
          nodeRuntime  <- Gateway
                            .compose(Subgraph.graphql("nodes", remote.endpoint, limitsSchema))
                            .withConfig(_.withMaxParsedOperationNodes(3))
                            .interpreter
          nodes        <- nodeRuntime.execute("{ a b c }")
          sent         <- remote.requests.get
        } yield assertTrue(
          text.errors.map(_.msg) == List("Operation text exceeded the configured byte limit."),
          depth.errors.map(_.msg) == List("Operation nesting exceeded the configured limit."),
          nodes.errors.map(_.msg) == List("Operation structure exceeded the configured node limit."),
          sent.isEmpty
        )
      },
      test("rejects invalid gateway bounds before constructing a runtime") {
        for {
          exit <- Gateway
                    .compose(Subgraph.local("local", localValueGraph(ZIO.succeed("ok"))))
                    .withConfig(
                      _.withMaxOperationNesting(0)
                        .withMaxPlanningCandidates(0)
                        .withMaxPlanningExpansions(0)
                        .withPlanningTimeout(Duration.Infinity)
                        .withMaxConcurrentRequests(0)
                    )
                    .interpreter
                    .exit
        } yield assertTrue(
          buildDiagnostics(exit) == List(
            "Gateway maxOperationNesting must be positive.",
            "Gateway maxPlanningCandidates must be positive.",
            "Gateway maxPlanningExpansions must be positive.",
            "Gateway planning timeout must be finite and positive.",
            "Gateway maxConcurrentRequests must be positive."
          )
        )
      },
      test("ignores GraphQL string and comment contents when measuring nesting") {
        val limits       = new OperationParsingLimits(1024, 2, 100)
        val escapedBlock =
          "query { value(input: " + "\"\"\"" + "ignored \\\"\"\" { [ ( " + "\"\"\"" + ") }"
        val ordinary     = "query { value(input: \"{[(\") } # {[(("

        assertTrue(limits.textBytes(escapedBlock).isRight, limits.textBytes(ordinary).isRight)
      }
    ),
    suite("admission")(
      test("removes interrupted request waiters and releases the request permit") {
        for {
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          calls   <- Ref.make(0)
          effect   = calls.updateAndGet(_ + 1).flatMap { index =>
                       if (index == 1) started.succeed(()).unit *> release.await.as("first")
                       else ZIO.succeed("next")
                     }
          runtime <- Gateway
                       .compose(Subgraph.local("local", localValueGraph(effect)))
                       .withConfig(_.withMaxConcurrentRequests(1))
                       .interpreter
          first   <- runtime.execute("{ localValue }").fork
          _       <- started.await
          second  <- runtime.execute("{ localValue }").fork
          waiting <- waitForStatus(runtime)(_.requests.waiting == 1)
          exit    <- second.interrupt
          after   <- waitForStatus(runtime)(_.requests.waiting == 0)
          _       <- release.succeed(())
          _       <- first.join
          third   <- runtime.execute("{ localValue }")
          count   <- calls.get
          done    <- runtime.status
        } yield assertTrue(
          waiting.requests.active == 1,
          exit.isInterrupted,
          after.requests.active == 1,
          field(third.data, "localValue").contains(StringValue("next")),
          count == 2,
          done.requests.active == 0,
          done.requests.waiting == 0
        )
      },
      test("applies request admission to explain planning") {
        for {
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          calls   <- Ref.make(0)
          resolver = OperationResolver[Any](_ =>
                       calls.updateAndGet(_ + 1).flatMap { index =>
                         if (index == 1) started.succeed(()).unit *> release.await.as("{ localValue }")
                         else ZIO.succeed("{ localValue }")
                       }
                     )
          runtime <- Gateway
                       .compose(Subgraph.local("local", localValueGraph(ZIO.succeed("value"))))
                       .withOperationResolver(resolver)
                       .withConfig(_.withMaxConcurrentRequests(1))
                       .interpreter
          first   <- runtime.explain(GraphQLRequest()).fork
          _       <- started.await
          second  <- runtime.explain(GraphQLRequest()).fork
          waiting <- waitForStatus(runtime)(_.requests.waiting == 1)
          exit    <- second.interrupt
          _       <- release.succeed(())
          plan    <- first.join
          done    <- runtime.status
        } yield assertTrue(
          waiting.requests.active == 1,
          exit.isInterrupted,
          plan.contains("fetch local"),
          done.requests.active == 0,
          done.requests.waiting == 0
        )
      },
      test("applies request admission to validation checks") {
        for {
          started  <- Promise.make[Nothing, Unit]
          release  <- Promise.make[Nothing, Unit]
          runtime  <- Gateway
                        .compose(
                          Subgraph.local(
                            "local",
                            localValueGraph(started.succeed(()).unit *> release.await.as("value"))
                          )
                        )
                        .withConfig(_.withMaxConcurrentRequests(1))
                        .interpreter
          running  <- runtime.execute("{ localValue }").fork
          _        <- started.await
          checking <- runtime.check("{ localValue }").fork
          waiting  <- waitForStatus(runtime)(_.requests.waiting == 1)
          exit     <- checking.interrupt
          _        <- release.succeed(())
          _        <- running.join
          done     <- runtime.status
        } yield assertTrue(
          waiting.requests.active == 1,
          exit.isInterrupted,
          done.requests.active == 0,
          done.requests.waiting == 0
        )
      },
      test("runs local and remote sources concurrently with independent permits") {
        for {
          localStarted  <- Promise.make[Nothing, Unit]
          remoteStarted <- Promise.make[Nothing, Unit]
          release       <- Promise.make[Nothing, Unit]
          remote        <- stubWith(remoteStarted.succeed(()).unit *> release.await, response)
          runtime       <- Gateway
                             .compose(
                               Subgraph.local(
                                 "local",
                                 localValueGraph(localStarted.succeed(()).unit *> release.await.as("local"))
                               ),
                               Subgraph.graphql("remote", remote.endpoint, schema)
                             )
                             .withConfig(
                               _.withMaxConcurrentRequests(2)
                                 .withMaxConcurrentLocalCalls(1)
                             )
                             .interpreter
          fiber         <- runtime.execute("{ localValue value }").fork
          _             <- localStarted.await.zipPar(remoteStarted.await)
          status        <- runtime.status
          _             <- release.succeed(())
          result        <- fiber.join
        } yield assertTrue(
          status.requests.active == 1,
          status.subgraphs.get("local").exists(value => value.active == 1 && value.limit == 1),
          status.subgraphs.get("remote").exists(_.active == 1),
          field(result.data, "localValue").contains(StringValue("local")),
          field(result.data, "value").contains(StringValue("ok"))
        )
      },
      test("holds one source permit across retry attempts") {
        val config = RemoteGraphQLConfig.default.withExecution(
          _.withRetries(1, Duration.Zero)
            .withMaxConcurrentCalls(1)
        )
        for {
          calls        <- Ref.make(0)
          retryStarted <- Promise.make[Nothing, Unit]
          releaseRetry <- Promise.make[Nothing, Unit]
          remote       <- endpoint { _ =>
                            calls.updateAndGet(_ + 1).flatMap {
                              case 1 => ZIO.succeed(Response.status(Status.ServiceUnavailable))
                              case 2 => retryStarted.succeed(()).unit *> releaseRetry.await.as(graphQLResponse(response))
                              case _ => ZIO.succeed(graphQLResponse(response))
                            }
                          }
          runtime      <- Gateway
                            .compose(Subgraph.graphql("remote", remote, schema, config))
                            .withConfig(_.withMaxConcurrentRequests(2))
                            .interpreter
          first        <- runtime.executeRequest(request).fork
          _            <- retryStarted.await
          second       <- runtime.executeRequest(request).fork
          queued       <- waitForStatus(runtime)(_.subgraphs.get("remote").exists(_.waiting == 1))
          before       <- calls.get
          _            <- releaseRetry.succeed(())
          firstResult  <- first.join
          secondResult <- second.join
          total        <- calls.get
          done         <- runtime.status
        } yield assertTrue(
          queued.subgraphs.get("remote").exists(value => value.active == 1 && value.waiting == 1),
          before == 2,
          firstResult.errors.isEmpty,
          secondResult.errors.isEmpty,
          total == 3,
          done.subgraphs.get("remote").exists(value => value.active == 0 && value.waiting == 0)
        )
      },
      test("deduplicates identical queries before source admission") {
        val config = RemoteGraphQLConfig.default.withExecution(
          _.withMaxConcurrentCalls(1)
            .withInFlightQueryDeduplication(true)
        )
        for {
          calls       <- Ref.make(0)
          started     <- Promise.make[Nothing, Unit]
          release     <- Promise.make[Nothing, Unit]
          remote      <- endpoint(_ =>
                           calls.update(_ + 1) *>
                             started.succeed(()).unit *>
                             release.await.as(graphQLResponse(response))
                         )
          runtime     <- (Gateway
                           .compose(Subgraph.graphql("remote", remote, schema, config))
                           .withConfig(_.withMaxConcurrentRequests(32)) @@ GatewayMetrics.wrapper).interpreter
          startBefore <- counter("caliban_gateway_in_flight_deduplication_total", "result", "start")
          joinBefore  <- counter("caliban_gateway_in_flight_deduplication_total", "result", "join")
          fibers      <- ZIO.foreach(1 to 20)(_ => runtime.executeRequest(request).fork)
          _           <- started.await
          sharing     <- waitForStatus(runtime)(_.requests.active == 20)
          before      <- calls.get
          _           <- release.succeed(())
          responses   <- ZIO.foreach(fibers)(_.join)
          done        <- runtime.status
          startAfter  <- counter("caliban_gateway_in_flight_deduplication_total", "result", "start")
          joinAfter   <- counter("caliban_gateway_in_flight_deduplication_total", "result", "join")
        } yield assertTrue(
          before == 1,
          sharing.subgraphs.get("remote").exists(value => value.active == 1 && value.waiting == 0),
          startAfter == startBefore + 1.0,
          joinAfter == joinBefore + 19.0,
          responses.forall(_.errors.isEmpty),
          done.subgraphs.get("remote").exists(value => value.active == 0 && value.waiting == 0)
        )
      },
      test("bounds distinct deduplication identities before source admission") {
        val config       = RemoteGraphQLConfig.default.withExecution(
          _.withMaxConcurrentCalls(1)
            .withInFlightQueryDeduplication(true)
        )
        val operations   = Some("query First { value } query Second { value }")
        val firstRequest = GraphQLRequest(query = operations, operationName = Some("First"))
        val nextRequest  = GraphQLRequest(query = operations, operationName = Some("Second"))
        for {
          calls        <- Ref.make(0)
          firstStarted <- Promise.make[Nothing, Unit]
          nextStarted  <- Promise.make[Nothing, Unit]
          releaseFirst <- Promise.make[Nothing, Unit]
          remote       <- endpoint(_ =>
                            calls.updateAndGet(_ + 1).flatMap {
                              case 1 => firstStarted.succeed(()).unit *> releaseFirst.await.as(graphQLResponse(response))
                              case _ => nextStarted.succeed(()).as(graphQLResponse(response))
                            }
                          )
          runtime      <- Gateway
                            .compose(Subgraph.graphql("remote", remote, schema, config))
                            .withConfig(_.withMaxConcurrentRequests(2))
                            .interpreter
          first        <- runtime.executeRequest(firstRequest).fork
          _            <- firstStarted.await
          second       <- runtime.executeRequest(nextRequest).fork
          bounded      <- waitForStatus(runtime)(_.requests.active == 2)
          before       <- calls.get
          _            <- releaseFirst.succeed(())
          _            <- nextStarted.await
          responses    <- first.join.zip(second.join)
          total        <- calls.get
        } yield assertTrue(
          before == 1,
          bounded.subgraphs.get("remote").exists(value => value.active == 1 && value.waiting == 0),
          responses._1.errors.isEmpty,
          responses._2.errors.isEmpty,
          total == 2
        )
      },
      test("releases a source permit when the current call is interrupted") {
        for {
          gate          <- AdmissionGate.make(1, GatewayWrapper.AdmissionKind.Request)
          firstStarted  <- Promise.make[Nothing, Unit]
          secondStarted <- Promise.make[Nothing, Unit]
          first         <- gate(firstStarted.succeed(()).unit *> ZIO.never).fork
          _             <- firstStarted.await
          second        <- gate(secondStarted.succeed(()).unit).fork
          waiting       <- gate.status.repeatUntil(_.waiting == 1)
          firstExit     <- first.interrupt
          _             <- secondStarted.await
          _             <- second.join
          done          <- gate.status
        } yield assertTrue(
          waiting.active == 1,
          firstExit.isInterrupted,
          done.active == 0,
          done.waiting == 0
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
