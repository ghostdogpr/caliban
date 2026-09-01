package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.Allow
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal._
import caliban.validation.Validator
import caliban.{ CalibanError, Configurator, GraphQLRequest, InputValue }
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
        } yield assertTrue(
          values.forall(_ == 1),
          runs == 1
        )
      },
      test("evicts entries by total weight") {
        for {
          cache <- OperationCache.make[String, String, Int, Any](5, GatewayWrapper.empty)
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
          recorded         <- recordEvents
          (events, wrapper) = recorded
          cache            <- OperationCache.make[String, String, Int, Any](32, wrapper)
          computing        <- Promise.make[Nothing, Unit]
          release          <- Promise.make[Nothing, Unit]
          computations     <- Ref.make(0)
          leader           <- cache
                                .getOrCompute("same")(
                                  computations.update(_ + 1) *>
                                    computing.succeed(()).unit *>
                                    release.await.as(Weighted(1, 4))
                                )
                                .fork
          _                <- computing.await
          waiter           <- cache.getOrCompute("same")(ZIO.dieMessage("waiter computed")).fork
          _                <- events.get.repeatUntil(_.contains(GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Wait)))
          waiterExit       <- waiter.interrupt
          _                <- release.succeed(())
          leaderValue      <- leader.join
          cached           <- cache.getOrCompute("same")(ZIO.dieMessage("cache missed"))
          runs             <- computations.get
        } yield assertTrue(waiterExit.isInterrupted, leaderValue == 1, cached == 1, runs == 1)
      },
      test("cleans up an in-flight entry when the miss wrapper interrupts") {
        for {
          interrupt <- Ref.make(true)
          wrapper    = new GatewayWrapper[Any] {
                         def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
                           result: Exit[E, A] => GatewayWrapper.Result
                         )(implicit trace: Trace): ZIO[R0, E, A] =
                           event match {
                             case GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss) =>
                               interrupt.getAndSet(false).flatMap(if (_) ZIO.interrupt else effect)
                             case _                                                                 => effect
                           }
                       }
          cache     <- OperationCache.make[String, String, Int, Any](32, wrapper)
          first     <- cache.getOrCompute("same")(ZIO.succeed(Weighted(1, 4))).exit
          second    <- cache.getOrCompute("same")(ZIO.succeed(Weighted(2, 4)))
        } yield assertTrue(
          first.isInterrupted,
          second == 2
        )
      },
      test("retries a waiter when the compute leader is interrupted before computation starts") {
        for {
          firstMiss  <- Ref.make(true)
          entered    <- Promise.make[Nothing, Unit]
          joined     <- Promise.make[Nothing, Unit]
          wrapper     = new GatewayWrapper[Any] {
                          def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
                            result: Exit[E, A] => GatewayWrapper.Result
                          )(implicit trace: Trace): ZIO[R0, E, A] =
                            event match {
                              case GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss) =>
                                firstMiss.getAndSet(false).flatMap {
                                  case true  => entered.succeed(()).unit *> ZIO.never
                                  case false => effect
                                }
                              case GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Wait) =>
                                joined.succeed(()).unit *> effect
                              case _                                                                 => effect
                            }
                        }
          cache      <- OperationCache.make[String, String, Int, Any](32, wrapper)
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
      test("caches prepared plans independently of policy evaluation") {
        for {
          recorded         <- recordEvents
          (events, wrapper) = recorded
          policyCalls      <- Ref.make(0)
          stableRemote     <- stub(response)
          stable           <- (Gateway
                                .compose(Subgraph.graphql("stable", stableRemote.endpoint, schema))
                                .withOperationPolicy(
                                  OperationPolicy[Any](_ => policyCalls.update(_ + 1).as(Allow))
                                ) @@ wrapper).interpreter
          _                <- stable.executeRequest(request)
          _                <- stable.executeRequest(request)
          policyRuns       <- policyCalls.get
          observed         <- events.get
        } yield assertTrue(
          observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == 1,
          policyRuns == 2
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
          recorded         <- recordEvents
          (events, wrapper) = recorded
          gateway           = Gateway.compose(Subgraph.local("local", localGraph(ZIO.succeed("ok")))) @@ wrapper
          runtime          <- gateway.interpreter
          first            <- execute(runtime, allowed)
          second           <- execute(runtime, allowed.map(identity))
          rejected         <- execute(runtime, denied)
          restored         <- execute(runtime, allowed)
          other            <- gateway.interpreter
          otherRejected    <- execute(other, denied)
          observed         <- events.get
          accesses          = observed.collect { case GatewayWrapper.Event.CacheAccess(result) => result }
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
          restored.errors.isEmpty,
          rejected.errors.map(_.msg) == List("Custom validation rejected."),
          otherRejected.errors.map(_.msg) == List("Custom validation rejected."),
          accesses == Vector(
            GatewayWrapper.CacheResult.Miss,
            GatewayWrapper.CacheResult.Hit,
            GatewayWrapper.CacheResult.Miss,
            GatewayWrapper.CacheResult.Hit,
            GatewayWrapper.CacheResult.Miss
          )
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
          forwarded      <- remote.requests.get
          disabledResults = disabledSchema :: disabledType :: Nil
        } yield assertTrue(
          enabledMiss.errors.isEmpty,
          enabledHit.errors.isEmpty,
          reenabled.errors.isEmpty,
          disabledResults.forall(_.errors.map(_.msg) == List("Introspection is disabled")),
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
        } yield assertTrue(
          first.errors.isEmpty,
          second.errors.isEmpty,
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
        } yield assertTrue(
          !sent.headOption.flatMap(_.query).exists(_.contains("conditionalValue")),
          sent.drop(1).headOption.flatMap(_.query).exists(_.contains("conditionalValue"))
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
        } yield assertTrue(
          multipleFields.errors.nonEmpty,
          nullField.errors.nonEmpty,
          sent.isEmpty
        )
      },
      test("rejects invalid gateway bounds before constructing a runtime") {
        for {
          exit <- Gateway
                    .compose(Subgraph.local("local", localValueGraph(ZIO.succeed("ok"))))
                    .withConfig(
                      _.withMaxPlanningCandidates(0)
                        .withMaxPlanningExpansions(0)
                        .withPlanningTimeout(Duration.Infinity)
                        .withMaxConcurrentRequests(0)
                    )
                    .interpreter
                    .exit
        } yield assertTrue(
          buildDiagnostics(exit) == List(
            "Gateway maxPlanningCandidates must be positive.",
            "Gateway maxPlanningExpansions must be positive.",
            "Gateway planning timeout must be finite and positive.",
            "Gateway maxConcurrentRequests must be positive."
          )
        )
      }
    ),
    suite("admission")(
      test("holds one observed request admission across preparation and execution") {
        for {
          recorded         <- recordEvents
          (_, wrapper)      = recorded
          routingCalls     <- Ref.make(0)
          firstRouting     <- Promise.make[Nothing, Unit]
          releaseRouting   <- Promise.make[Nothing, Unit]
          secondRouting    <- Promise.make[Nothing, Unit]
          executionStarted <- Promise.make[Nothing, Unit]
          releaseExecution <- Promise.make[Nothing, Unit]
          resolver          = OperationResolver[Any](_ =>
                                routingCalls.updateAndGet(_ + 1).flatMap {
                                  case 1 =>
                                    firstRouting.succeed(()).unit *> releaseRouting.await.as("{ localValue }")
                                  case _ => secondRouting.succeed(()).as("{ localValue }")
                                }
                              )
          runtime          <- (Gateway
                                .compose(
                                  Subgraph.local(
                                    "local",
                                    localValueGraph(
                                      executionStarted.succeed(()).unit *> releaseExecution.await.as("ok")
                                    )
                                  )
                                )
                                .withOperationResolver(resolver)
                                .withConfig(_.withMaxConcurrentRequests(1)) @@ wrapper).interpreter
          first            <- runtime.executeRequest(GraphQLRequest()).fork
          _                <- firstRouting.await
          second           <- runtime.executeRequest(GraphQLRequest()).fork
          _                <- TestClock.adjust(Duration.Zero)
          _                <- releaseRouting.succeed(())
          next             <- executionStarted.await.as("execution").race(secondRouting.await.as("routing"))
          _                <- releaseExecution.succeed(())
          responses        <- first.join.zip(second.join)
        } yield assertTrue(
          next == "execution",
          responses._1.errors.isEmpty,
          responses._2.errors.isEmpty
        )
      },
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
          _       <- TestClock.adjust(Duration.Zero)
          exit    <- second.interrupt
          _       <- release.succeed(())
          _       <- first.join
          third   <- runtime.execute("{ localValue }")
          count   <- calls.get
        } yield assertTrue(
          exit.isInterrupted,
          field(third.data, "localValue").contains(StringValue("next")),
          count == 2
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
          _       <- TestClock.adjust(Duration.Zero)
          exit    <- second.interrupt
          _       <- release.succeed(())
          plan    <- first.join
        } yield assertTrue(
          exit.isInterrupted,
          plan.contains("fetch local")
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
          _        <- TestClock.adjust(Duration.Zero)
          exit     <- checking.interrupt
          _        <- release.succeed(())
          _        <- running.join
        } yield assertTrue(
          exit.isInterrupted
        )
      },
      test("local calls share only the gateway request budget") {
        val concurrency = 65
        for {
          started <- Ref.make(0)
          release <- Promise.make[Nothing, Unit]
          runtime <-
            Gateway
              .compose(Subgraph.local("local", localValueGraph(started.update(_ + 1) *> release.await.as("ok"))))
              .withConfig(_.withMaxConcurrentRequests(concurrency))
              .interpreter
          fibers  <- ZIO.foreach(1 to concurrency)(_ => runtime.execute("{ localValue }").fork)
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
      test("holds one source permit across retry attempts") {
        val config = RemoteGraphQLConfig.default.withExecution(
          _.withRetries(1, Duration.Zero)
            .withMaxConcurrentCalls(1)
            .withInFlightQueryDeduplication(false)
        )
        for {
          recorded         <- recordEvents
          (events, wrapper) = recorded
          calls            <- Ref.make(0)
          retryStarted     <- Promise.make[Nothing, Unit]
          releaseRetry     <- Promise.make[Nothing, Unit]
          remote           <- endpoint { _ =>
                                calls.updateAndGet(_ + 1).flatMap {
                                  case 1 => ZIO.succeed(Response.status(Status.ServiceUnavailable))
                                  case 2 => retryStarted.succeed(()).unit *> releaseRetry.await.as(graphQLResponse(response))
                                  case _ => ZIO.succeed(graphQLResponse(response))
                                }
                              }
          runtime          <- (Gateway
                                .compose(Subgraph.graphql("remote", remote, schema, config))
                                .withConfig(_.withMaxConcurrentRequests(2)) @@ wrapper).interpreter
          first            <- runtime.executeRequest(request).fork
          _                <- retryStarted.await
          second           <- runtime.executeRequest(request).fork
          _                <- events.get.repeatUntil(
                                _.count(_.isInstanceOf[GatewayWrapper.Event.SubgraphCall]) == 2
                              )
          _                <- TestClock.adjust(Duration.Zero)
          before           <- calls.get
          _                <- releaseRetry.succeed(())
          firstResult      <- first.join
          secondResult     <- second.join
          total            <- calls.get
        } yield assertTrue(
          before == 2,
          firstResult.errors.isEmpty,
          secondResult.errors.isEmpty,
          total == 3
        )
      },
      test("deduplicates identical queries before source admission") {
        val config = RemoteGraphQLConfig.default.withExecution(_.withMaxConcurrentCalls(1))
        for {
          calls     <- Ref.make(0)
          started   <- Promise.make[Nothing, Unit]
          release   <- Promise.make[Nothing, Unit]
          remote    <- endpoint(_ =>
                         calls.update(_ + 1) *>
                           started.succeed(()).unit *>
                           release.await.as(graphQLResponse(response))
                       )
          runtime   <- (Gateway
                         .compose(Subgraph.graphql("remote", remote, schema, config))
                         .withConfig(_.withMaxConcurrentRequests(32)) @@ GatewayMetrics.wrapper).interpreter
          fibers    <- ZIO.foreach(1 to 20)(_ => runtime.executeRequest(request).fork)
          _         <- started.await
          _         <- TestClock.adjust(Duration.Zero)
          before    <- calls.get
          _         <- release.succeed(())
          responses <- ZIO.foreach(fibers)(_.join)
          total     <- calls.get
        } yield assertTrue(
          before == 1,
          total == 1,
          responses.forall(_.errors.isEmpty)
        )
      },
      test("bounds distinct deduplication identities before source admission") {
        val config       = RemoteGraphQLConfig.default.withExecution(_.withMaxConcurrentCalls(1))
        val operations   = Some("query First { value } query Second { value }")
        val firstRequest = GraphQLRequest(query = operations, operationName = Some("First"))
        val nextRequest  = GraphQLRequest(query = operations, operationName = Some("Second"))
        for {
          recorded         <- recordEvents
          (events, wrapper) = recorded
          calls            <- Ref.make(0)
          firstStarted     <- Promise.make[Nothing, Unit]
          nextStarted      <- Promise.make[Nothing, Unit]
          releaseFirst     <- Promise.make[Nothing, Unit]
          remote           <- endpoint(_ =>
                                calls.updateAndGet(_ + 1).flatMap {
                                  case 1 => firstStarted.succeed(()).unit *> releaseFirst.await.as(graphQLResponse(response))
                                  case _ => nextStarted.succeed(()).as(graphQLResponse(response))
                                }
                              )
          runtime          <- (Gateway
                                .compose(Subgraph.graphql("remote", remote, schema, config))
                                .withConfig(_.withMaxConcurrentRequests(2)) @@ wrapper).interpreter
          first            <- runtime.executeRequest(firstRequest).fork
          _                <- firstStarted.await
          second           <- runtime.executeRequest(nextRequest).fork
          _                <- events.get.repeatUntil(_.count(_ == GatewayWrapper.Event.Routing) == 2)
          _                <- TestClock.adjust(Duration.Zero)
          before           <- calls.get
          _                <- releaseFirst.succeed(())
          _                <- nextStarted.await
          responses        <- first.join.zip(second.join)
          total            <- calls.get
        } yield assertTrue(
          before == 1,
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
          _             <- TestClock.adjust(Duration.Zero)
          blocked       <- secondStarted.isDone
          firstExit     <- first.interrupt
          _             <- secondStarted.await
          _             <- second.join
        } yield assertTrue(
          !blocked,
          firstExit.isInterrupted
        )
      }
    )
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
