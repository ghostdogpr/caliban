package caliban.gateway

import caliban._
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.{ GatewayExecutionControl, SubscriptionTermination }
import caliban.gateway.internal.execution.{ RemoteSubscription, SubgraphExecutor }
import caliban.schema.Schema.auto._
import caliban.ws.{ Protocol, WebSocketHooks }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import zio._
import zio.http._
import zio.metrics.Metric
import zio.stream.ZStream
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

object SubscriptionSpec extends ZIOSpecDefault {
  final case class Query(value: String)
  private val request = GraphQLRequest(query = Some("subscription { event }"))

  private def socketEndpoint(prefix: String)(
    handle: WebSocketChannel => Task[Unit]
  ): ZIO[Server with Ref[Int], Nothing, URL] =
    getEndpoint(prefix)(_ => Response.fromSocketApp(transportWsSocket(handle)))

  private def transportWsSocket(handle: WebSocketChannel => Task[Unit]): WebSocketApp[Any] =
    Handler.webSocket(handle).withConfig(WebSocketConfig.default.subProtocol(Some("graphql-transport-ws")))

  private def acknowledgeThen(channel: WebSocketChannel)(respond: PartialFunction[String, Task[Unit]]): Task[Unit] =
    channel.receiveAll {
      case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
        readFromString[GraphQLWSInput](text).`type` match {
          case "connection_init" =>
            channel.send(ChannelEvent.Read(WebSocketFrame.Text("""{"type":"connection_ack"}""")))
          case other             => respond.applyOrElse(other, (_: String) => ZIO.unit)
        }
      case _                                            => ZIO.unit
    }

  def spec = suite("SubscriptionSpec")(
    test("envelope errors retain public fields while terminal failures retain identity") {
      val failure  = SubscriptionTermination.Reload
      val event    = GraphQLResponse(Value.NullValue, List(failure))
      val response = GraphQLResponse(
        ResponseValue.StreamValue(ZStream.succeed(event.toResponseValue) ++ ZStream.fail(failure)),
        Nil
      )
      SubgraphExecutor.subscriptionResponses(response).either.runCollect.map { values =>
        val decoded  = values.collect { case Right(value) => value }
        val failures = values.collect { case Left(error) => error }
        assertTrue(
          decoded.map(_.toResponseValue) == Chunk(event.toResponseValue),
          decoded.flatMap(_.errors).collect { case error: CalibanError.ExecutionError =>
            SubscriptionTermination.isGatewayError(error)
          } == Chunk(false),
          failures.size == 1,
          failures.forall(_ eq failure)
        )
      }
    },
    test("incremental streams are not decoded as subscription envelopes for either hasNext value") {
      ZIO
        .foreach(List(true, false)) { hasNext =>
          val response = GraphQLResponse(
            ResponseValue.StreamValue(ZStream.dieMessage("must not consume an incremental stream as a subscription")),
            Nil,
            hasNext = Some(hasNext)
          )
          SubgraphExecutor
            .subscriptionResponses(response)
            .runCollect
            .map(values => assertTrue(values == Chunk(response)))
        }
        .map(_.reduce(_ && _))
    },
    test("failed or cancelled setup retains its slot until finalizers finish") {
      ZIO
        .foreach(List(false, true)) { cancel =>
          for {
            opened     <- Promise.make[Nothing, Unit]
            closing    <- Promise.make[Nothing, Unit]
            release    <- Promise.make[Nothing, Unit]
            work       <- GatewayExecutionControl.make(
                            GatewaySubscriptionConfig.default.withMaxActive(1),
                            PhaseHooks.empty,
                            30.seconds,
                            1.second
                          )
            control     = work.subscriptions
            open        = ZIO.addFinalizer(closing.succeed(()) *> release.await) *>
                            opened.succeed(()) *> (if (cancel) ZIO.never else ZIO.fail(SubscriptionTermination.Source))
            running    <- control.stream(open)(ZIO.succeed(_)).runDrain.exit.forkScoped
            _          <- opened.await
            cancelling <- (if (cancel) running.interrupt.unit else ZIO.unit).forkScoped
            _          <- closing.await
            rejected   <- control.stream(ZIO.succeed(ZStream.empty))(ZIO.succeed(_)).runDrain.exit
            _          <- TestClock.adjust(2.seconds)
            pending    <- running.poll
            _          <- release.succeed(())
            _          <- cancelling.join
            _          <- running.await
            next       <- control.stream(ZIO.succeed(ZStream.empty))(ZIO.succeed(_)).runDrain.exit
          } yield assertTrue(
            rejected.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Capacity),
            pending.isEmpty,
            next.isSuccess
          )
        }
        .map(_.reduce(_ && _))
    },
    test("setup and stream failures from the source report the same termination reason") {
      val boom    = new RuntimeException("boom")
      val remote  = CalibanError.ExecutionError("remote failure")
      val sources = List[ZIO[Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]]](
        ZIO.fail(boom),
        ZIO.die(boom),
        ZIO.succeed(ZStream.fail(boom)),
        ZIO.succeed(ZStream.fail(remote))
      )
      for {
        recorded     <- recordEvents
        (seen, hooks) = recorded
        work         <- GatewayExecutionControl.make(GatewaySubscriptionConfig.default, hooks, 30.seconds, 1.second)
        _            <- ZIO.foreachDiscard(sources)(open => work.subscriptions.stream(open)(ZIO.succeed(_)).runDrain.exit)
        observed     <- seen.get
      } yield assertTrue(
        observed.collect { case PhaseHooks.Event.SubscriptionTerminated(reason, _) => reason } ==
          Vector.fill(sources.size)("SUBSCRIPTION_SOURCE_ERROR")
      )
    },
    test("passthrough subscriptions preserve resolved directives and variables") {
      val schema    =
        "directive @trace(label: String!) on SUBSCRIPTION | FRAGMENT_DEFINITION type Query { value: String } type Subscription { event: Int }"
      val query     =
        """subscription Events($label: String!) @trace(label: $label) { ...Root } fragment Root on Subscription @trace(label: "fragment") { event }"""
      val variables = Some(Map("label" -> Value.StringValue("client")))
      for {
        sent     <- Ref.make(Option.empty[GraphQLRequest])
        endpoint <-
          postEndpoint("subscription-directive")(req =>
            req.body.asString.orDie
              .flatMap(body => sent.set(Some(readFromString[GraphQLRequest](body))))
              .as(graphQLResponse(sseBody(1), mediaType = "text/event-stream"))
          )
        runtime  <- remoteGateway(endpoint, schema, sseConfig)
                      .withPhaseHooks(PhaseHooks.resolution(_ => ZIO.succeed(query)))
                      .interpreter
        events   <-
          runtime
            .executeStream(
              GraphQLRequest(query = Some("persisted-id"), operationName = Some("Events"), variables = variables)
            )
            .runCollect
        captured <- sent.get
      } yield assertTrue(
        events.size == 1,
        captured.flatMap(_.query).contains(query),
        captured.flatMap(_.variables) == variables
      )
    },
    test("SSE heartbeats and ignored lines do not accumulate against the event size limit") {
      ZIO
        .foreach(List("\n", "\r", "\r\n")) { newline =>
          val ignored = s": heartbeat${newline}unknown: ignored${newline}event: next${newline}" * 40
          val body    = ignored + s"""data: {"data":$newline""" + ignored +
            s"""data: {"event":1}}$newline$newline""" + s"event: complete$newline$newline"
          for {
            endpoint <- sseEndpoint(body)
            runtime  <- remoteGateway(
                          endpoint,
                          subscriptionSchema,
                          sseConfig.withExecution(_.withMaxResponseBytes(128))
                        ).interpreter
            events   <- runtime.executeStream(request).runCollect
          } yield assertTrue(events.map(_.data.toString).toList == List("{\"event\":1}"))
        }
        .map(_.reduce(_ && _))
    },
    test("SSE comments do not reset the accumulated data limit") {
      val body = "event: next\n" + (("data: " + ("x" * 40) + "\n: heartbeat\n") * 4)
      for {
        endpoint <- sseEndpoint(body)
        runtime  <-
          remoteGateway(endpoint, subscriptionSchema, sseConfig.withExecution(_.withMaxResponseBytes(128))).interpreter
        exit     <- runtime.executeStream(request).runCollect.exit
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge)
      )
    },
    test("remote stream size limits preserve the termination code and observation") {
      for {
        recorded     <- recordEvents
        (seen, hooks) = recorded
        input        <- Queue.unbounded[String]
        first        <- Promise.make[Nothing, Unit]
        endpoint     <- streamingEndpoint(
                          ZStream
                            .fromQueue(input)
                            .flatMap(text => ZStream.fromIterable(text.getBytes(UTF_8))),
                          mediaType = "text/event-stream"
                        )
        config        = sseConfig.withExecution(_.withMaxResponseBytes(128))
        runtime      <- remoteGateway(endpoint, subscriptionSchema, config).withPhaseHooks(hooks).interpreter
        running      <- runtime.executeStream(request).tap(_ => first.succeed(())).runDrain.exit.forkScoped
        _            <- input.offer("event: next\ndata: {\"data\":{\"event\":1}}\n\n")
        _            <- first.await
        _            <- input.offer(":" + ("x" * 129))
        exit         <- running.join
        observed     <- seen.get
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge),
        observed.collect { case PhaseHooks.Event.SubscriptionTerminated(reason, _) => reason } ==
          Vector("SUBSCRIPTION_EVENT_TOO_LARGE")
      )
    },
    test("oversized WebSocket messages preserve the size-limit code, including fragmented UTF-8 payloads") {
      ZIO
        .foreach(List(false, true)) { fragmented =>
          val message = s"""{"type":"next","id":"1","payload":{"data":{"event":"${"é" * 100}"}}}"""
          val parts   = message.grouped(if (fragmented) 60 else message.length).toList
          for {
            recorded     <- recordEvents
            (seen, hooks) = recorded
            endpoint     <- socketEndpoint("subscription-ws-size")(channel =>
                              acknowledgeThen(channel) { case "subscribe" =>
                                ZIO.foreachDiscard(parts.zipWithIndex) { case (part, index) =>
                                  val last  = index == parts.size - 1
                                  val frame =
                                    if (index == 0) WebSocketFrame.Text(part, last)
                                    else WebSocketFrame.Continuation(Chunk.fromArray(part.getBytes(UTF_8)), last)
                                  channel.send(ChannelEvent.Read(frame))
                                }
                              }
                            )
            config        = RemoteGraphQLConfig.default.withExecution(_.withMaxResponseBytes(128))
            runtime      <-
              remoteGateway(endpoint, "type Query { value: String } type Subscription { event: String }", config)
                .withPhaseHooks(hooks)
                .interpreter
            exit         <- runtime.executeStream(request).runDrain.exit
            observed     <- seen.get
          } yield assertTrue(
            exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge),
            observed.collect { case PhaseHooks.Event.SubscriptionTerminated(reason, _) => reason } ==
              Vector("SUBSCRIPTION_EVENT_TOO_LARGE")
          )
        }
        .map(_.reduce(_ && _))
    },
    test("upstream buffer overflow keeps its code and reports the termination reason") {
      for {
        closed       <- Promise.make[Nothing, Unit]
        release      <- Promise.make[Nothing, Unit]
        recorded     <- recordEvents
        (seen, hooks) = recorded
        endpoint     <- socketEndpoint("subscription-ws-overflow")(channel =>
                          ZIO.scoped {
                            channel.awaitShutdown.ensuring(closed.succeed(())).forkScoped *>
                              acknowledgeThen(channel) { case "subscribe" =>
                                ZIO.foreachDiscard(1 to 2 * RemoteSubscription.BufferSize)(value =>
                                  channel.send(
                                    ChannelEvent.Read(
                                      WebSocketFrame.Text(
                                        s"""{"type":"next","id":"1","payload":{"data":{"event":$value}}}"""
                                      )
                                    )
                                  )
                                )
                              }
                          }
                        )
        runtime      <- remoteGateway(endpoint, subscriptionSchema)
                          .withPhaseHooks(
                            hooks ++ PhaseHooks.subscriptionSetup(PhaseHandler.outgoing((_, _) => release.await))
                          )
                          .interpreter
        running      <- runtime.executeStream(request).runDrain.exit.forkScoped
        _            <- closed.await
        _            <- release.succeed(())
        exit         <- running.join
        observations <- seen.get
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Overflow),
        observations.collect { case PhaseHooks.Event.SubscriptionTerminated(reason, _) => reason } == Vector(
          "SUBSCRIPTION_OVERFLOW"
        )
      )
    },
    test("pong timeout is independent of the longer keepalive interval") {
      for {
        first    <- Promise.make[Nothing, Unit]
        ping     <- Promise.make[Nothing, Unit]
        endpoint <- socketEndpoint("subscription-ws-pong")(channel =>
                      acknowledgeThen(channel) {
                        case "subscribe" =>
                          val parts = """{"type":"next","id":"1","payload":{"data":{"event":1}}}""".grouped(8).toList
                          ZIO.foreachDiscard(parts.zipWithIndex) { case (part, index) =>
                            val frame =
                              if (index == 0) WebSocketFrame.Text(part, false)
                              else
                                WebSocketFrame.Continuation(
                                  Chunk.fromArray(part.getBytes(UTF_8)),
                                  index == parts.size - 1
                                )
                            channel.send(ChannelEvent.Read(frame))
                          }
                        case "ping"      => ping.succeed(()).unit
                      }
                    )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      _.withKeepAliveInterval(60.seconds).withConnectionTimeout(1.second)
                    )
        runtime  <- remoteGateway(endpoint, subscriptionSchema, config).interpreter
        running  <- runtime.executeStream(request).tap(_ => first.succeed(())).runDrain.forkScoped
        _        <- first.await
        _        <- TestClock.adjust(60.seconds)
        _        <- ping.await
        _        <- TestClock.adjust(2.seconds)
        exit     <- Live.live(running.await.timeout(1.second))
        _        <- running.interrupt
      } yield assertTrue(
        exit.flatMap(_.causeOption).flatMap(_.failureOption).contains(SubscriptionTermination.Source)
      )
    },
    test("resolved subscriptions use executeRequest without an execution event") {
      for {
        recorded     <- recordEvents
        (seen, hooks) = recorded
        resolves     <- Ref.make(0)
        runtime      <-
          subscriptionGateway(ZStream(1, 2))
            .withPhaseHooks(
              PhaseHooks.resolution(_ => resolves.update(_ + 1).as("subscription { event }"))
            )
            .withPhaseHooks(hooks)
            .interpreter
        response     <- runtime.executeRequest(GraphQLRequest(query = Some("query { value }")))
        events       <- SubgraphExecutor.subscriptionResponses(response).runCollect
        observed     <- seen.get
        count        <- resolves.get
      } yield assertTrue(
        response.data.isInstanceOf[ResponseValue.StreamValue],
        events.map(_.data.toString).toList == List("{\"event\":1}", "{\"event\":2}"),
        !observed.exists(_.isInstanceOf[PhaseHooks.Event.Execution]),
        count == 1
      )
    },
    test("shutdown closes finite admission while subscription finalizers are still running") {
      for {
        opened   <- Promise.make[Nothing, Unit]
        closing  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        owner    <- Scope.make
        source    =
          ZStream.acquireReleaseWith(opened.succeed(()))(_ => closing.succeed(()) *> release.await) *> ZStream.never
        runtime  <- owner.extend(subscriptionGateway(source).withConfig(_.withDrainTimeout(1.second)).interpreter)
        running  <- runtime.executeStream(request).runDrain.exit.forkScoped
        _        <- opened.await
        stopping <- owner.close(Exit.unit).forkScoped
        _        <- closing.await
        _        <- TestClock.adjust(2.seconds)
        rejected <- runtime.execute("{ value }")
        pending  <- stopping.poll
        _        <- release.succeed(())
        _        <- stopping.join
        _        <- running.join
      } yield assertTrue(
        rejected.errors.nonEmpty,
        pending.isEmpty
      )
    },
    test("SSE GET supports BOM, CR line endings, and a distinct completion event") {
      val body = "\uFEFF: heartbeat\r\revent: next\rdata: {\"data\":{\"event\":1}}\r\revent: complete\r\r"
      for {
        sent     <- Ref.make(Option.empty[String])
        endpoint <- getEndpoint("subscription-get")(req =>
                      sent
                        .set(req.url.queryParams.getAll("query").headOption)
                        .as(graphQLResponse(body, mediaType = "text/event-stream"))
                    )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      _.withTransport(RemoteSubscriptionConfig.Sse(useGet = true))
                    )
        runtime  <- remoteGateway(endpoint, subscriptionSchema, config).interpreter
        events   <- runtime.executeStream(request).runCollect
        query    <- sent.get
      } yield assertTrue(
        events.map(_.data.toString).toList == List("{\"event\":1}"),
        query.exists(_.startsWith("subscription"))
      )
    },
    test("source completion drains every buffered event in order") {
      val values = (1 to 100).toList
      for {
        runtime <- subscriptionGateway(ZStream.fromIterable(values))
                     .withConfig(_.withSubscriptions(_.withBufferSize(128)))
                     .interpreter
        events  <- runtime.executeStream(request).runCollect
      } yield assertTrue(events.map(_.data.toString).toList == values.map(i => s"""{"event":$i}"""))
    },
    test("two subscriptions on one socket hold and release independent slots") {
      for {
        closed      <- Ref.make(0)
        source       = (ZStream.succeed(1) ++ ZStream.never).ensuring(closed.update(_ + 1))
        runtime     <- subscriptionGateway(source).interpreter
        input       <- Queue.unbounded[GraphQLWSInput]
        output      <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
        pipe        <-
          Protocol.GraphQLWS
            .make(runtime, None, WebSocketHooks.empty[Any, CalibanError])
        socket      <- pipe(ZStream.fromQueue(input)).runForeach(output.offer).forkScoped
        _           <- input.offer(GraphQLWSInput("connection_init", None, None))
        _           <- output.take
        _           <- ZIO.foreachDiscard(List("1", "2"))(id =>
                         input.offer(
                           GraphQLWSInput(
                             "subscribe",
                             Some(id),
                             Some(InputValue.ObjectValue(Map("query" -> Value.StringValue("subscription { event }"))))
                           )
                         )
                       )
        _           <- output.take.repeatN(1)
        _           <- input.offer(GraphQLWSInput("complete", Some("1"), None))
        _           <- closed.get.repeatUntil(_ == 1)
        firstClosed <- closed.get
        _           <- input.offer(GraphQLWSInput("complete", Some("2"), None))
        _           <- closed.get.repeatUntil(_ == 2)
        allClosed   <- closed.get
        _           <- socket.interrupt
      } yield assertTrue(firstClosed == 1, allClosed == 2)
    },
    test("captures configured headers before streaming and edits them when opening the subgraph") {
      for {
        identity    <- FiberRef.make("later")
        headers     <- Ref.make(List.empty[String])
        multi       <- Ref.make(List.empty[String])
        policies    <- Ref.make(0)
        headerCalls <- Ref.make(0)
        hookCalls   <- Ref.make(0)
        endpoint    <- postEndpoint("subscription-identity")(req =>
                         headers
                           .update(_ ++ req.headers.get("X-Identity").toList)
                           .zipRight(multi.set(renderedHeaderValues(req.headers, "X-Multi")))
                           .as(graphQLResponse(sseBody(1, 2), mediaType = "text/event-stream"))
                       )
        config       =
          sseConfig
            .withExecutionHeadersZIO(
              headerCalls.update(_ + 1) *> identity.get.map(value =>
                List(
                  Header.Custom("X-Identity", value),
                  Header.Custom("X-Multi", "first"),
                  Header.Custom("X-Multi", "second")
                )
              )
            )
        runtime     <-
          remoteGateway(endpoint, subscriptionSchema, config)
            .withPhaseHooks(
              PhaseHooks.authorization[Any](_ => policies.update(_ + 1).unit)
            )
            .withPhaseHooks(
              PhaseHooks.subgraphCall(
                PhaseHandler.incoming[Any, PhaseHooks.Event.SubgraphCall, Nothing](event =>
                  hookCalls.update(_ + 1).as(event.copy(headers = event.headers :+ Header.Custom("X-Multi", "hook")))
                )
              )
            )
            .interpreter
        response    <- identity.locally("captured")(runtime.executeRequest(request))
        events      <- SubgraphExecutor.subscriptionResponses(response).runCollect
        sent        <- headers.get
        multiValues <- multi.get
        calls       <- headerCalls.get
        checks      <- policies.get
        edits       <- hookCalls.get
      } yield assertTrue(
        events.size == 2,
        sent == List("captured"),
        multiValues == List("first, second, hook"),
        calls == 1,
        checks == 1,
        edits == 1
      )
    },
    test("idle subscriptions ignore the ordinary request timeout") {
      for {
        opened    <- Promise.make[Nothing, Unit]
        runtime   <- subscriptionGateway(ZStream.fromZIO(opened.succeed(())) *> ZStream.never)
                       .withConfig(
                         _.withRequestTimeout(1.second)
                       )
                       .interpreter
        running   <- runtime.executeStream(request).runDrain.exit.forkScoped
        _         <- opened.await
        _         <- TestClock.adjust(2.seconds)
        idle      <- running.poll
        _         <- TestClock.adjust(8.seconds)
        stillIdle <- running.poll
        _         <- running.interrupt
      } yield assertTrue(idle.isEmpty, stillIdle.isEmpty)
    },
    test("ordinary interpreter middleware preserves subscriptions and its response transformation") {
      for {
        opened   <- Ref.make(0)
        runtime  <- subscriptionGateway(ZStream.fromZIO(opened.updateAndGet(_ + 1))).interpreter
        wrapped   =
          runtime.wrapExecutionWith(
            _.map(_.copy(extensions = Some(ResponseValue.ObjectValue(List("wrapped" -> Value.BooleanValue(true))))))
          )
        response <- wrapped.executeRequest(request)
        count    <- opened.get
      } yield assertTrue(
        response.errors.isEmpty,
        response.data.isInstanceOf[ResponseValue.StreamValue],
        response.hasNext.isEmpty,
        response.extensions.nonEmpty,
        count == 0
      )
    },
    test("overflow sheds the operation and records its termination") {
      for {
        queue        <- Queue.unbounded[Int]
        processing   <- Promise.make[Nothing, Unit]
        recorded     <- recordEvents
        (seen, hooks) = recorded
        stalled       = PhaseHooks.subscriptionEvent(
                          PhaseHandler.incomingDiscard(_ => processing.succeed(()).unit *> ZIO.never)
                        )
        runtime      <- subscriptionGateway(ZStream.fromQueue(queue))
                          .withConfig(_.withSubscriptions(_.withBufferSize(1)))
                          .withPhaseHooks(hooks ++ stalled ++ GatewayMetrics.hooks)
                          .interpreter
        before       <- Metric.counter("caliban_gateway_subscription_overflows_total").value.map(_.count)
        running      <- runtime.executeStream(request).runDrain.exit.forkScoped
        _            <- queue.offer(1)
        _            <- processing.await
        _            <- queue.offerAll(List(2, 3, 4))
        exit         <- running.join
        events       <- seen.get
        after        <- Metric.counter("caliban_gateway_subscription_overflows_total").value.map(_.count)
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Overflow),
        after == before + 1L,
        events.collect { case PhaseHooks.Event.SubscriptionTerminated(reason, _) => reason } == Vector(
          "SUBSCRIPTION_OVERFLOW"
        ),
        !events.exists(_.isInstanceOf[PhaseHooks.Event.Execution])
      )
    },
    test("local sources retain native field-stream error behavior") {
      final case class Event(value: IO[Throwable, Option[String]])
      final case class Events(event: ZStream[Any, Throwable, Event])
      val api     = graphQL(
        RootResolver(
          queryResolver = Some(Query("ok")),
          mutationResolver = Option.empty[Unit],
          subscriptionResolver = Some(
            Events(
              ZStream(
                Event(ZIO.fail(new RuntimeException("bad event"))),
                Event(ZIO.succeed(Some("next")))
              )
            )
          )
        )
      )
      val request = GraphQLRequest(query = Some("subscription { event { value } }"))
      for {
        source   <- api.interpreter
        response <- source.executeRequest(request)
        native   <- SubgraphExecutor.subscriptionResponses(response).runCollect
        runtime  <- Gateway.compose(Subgraph.graphql("local", api)).interpreter
        events   <- runtime.executeStream(request).runCollect
      } yield assertTrue(
        events == native,
        events.size == 2,
        events.head.data.toString == "{\"event\":{\"value\":null}}",
        events.last.data.toString == "{\"event\":{\"value\":\"next\"}}",
        events.forall(_.errors.isEmpty)
      )
    },
    test("idle subscription holds one slot without blocking queries; cancellation awaits source cleanup") {
      for {
        opened   <- Promise.make[Nothing, Unit]
        closing  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        source    =
          ZStream.acquireReleaseWith(opened.succeed(()))(_ => closing.succeed(()) *> release.await) *> (ZStream.succeed(
            1
          ) ++ ZStream.never)
        runtime  <-
          subscriptionGateway(source)
            .withConfig(_.withSubscriptions(_.withMaxActive(1)))
            .interpreter
        running  <- runtime.executeStream(request).runDrain.forkScoped
        _        <- opened.await
        rejected <- runtime.executeStream(request).runDrain.exit
        query    <- runtime.execute("{ value }")
        stopping <- running.interrupt.forkScoped
        _        <- closing.await
        during   <- runtime.executeStream(request).runDrain.exit
        pending  <- stopping.poll
        _        <- release.succeed(())
        _        <- stopping.join
        next     <- runtime.executeStream(request).take(1).runCollect
      } yield assertTrue(
        rejected.isFailure,
        query.errors.isEmpty,
        during.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Capacity),
        pending.isEmpty,
        next.size == 1
      )
    },
    test("constructing a stream acquires nothing and a closed gateway rejects consumption") {
      for {
        opens        <- Ref.make(0)
        gatewayScope <- Scope.make
        runtime      <- gatewayScope.extend(subscriptionGateway(ZStream.fromZIO(opens.updateAndGet(_ + 1))).interpreter)
        result        = runtime.executeStream(request)
        _            <- gatewayScope.close(Exit.unit)
        responses    <- result.runCollect
        count        <- opens.get
      } yield assertTrue(
        responses.head.errors.nonEmpty,
        count == 0
      )
    },
    test("SSE source preserves data and redacts errors and response extensions") {
      val body =
        "event: next\ndata: {\"data\":{\"event\":1},\"errors\":[{\"message\":\"secret\",\"path\":[\"event\"],\"extensions\":{\"secret\":true}},{\"message\":\"second secret\",\"path\":[\"event\"]}],\"extensions\":{\"secret\":true}}\n\nevent: complete\n\n"
      for {
        endpoint <- sseEndpoint(body)
        runtime  <- remoteGateway(endpoint, subscriptionSchema, sseConfig).interpreter
        events   <- runtime.executeStream(request).runCollect
      } yield assertTrue(
        events.size == 1,
        events.head.errors.size == 2,
        !events.head.toString.contains("secret"),
        events.head.extensions.isEmpty
      )
    },
    test("hydrates every source event with fresh entity results") {
      val products =
        productsFederationSchema.replace("{ query: Query }", "{ query: Query subscription: Subscription }") +
          " type Subscription { changed: Product }"
      val body     = List("first", "second")
        .map(name =>
          "event: next\ndata: " + s"""{"data":{"changed":{"_caliban_gateway_typename":"Product","_caliban_gateway_key":"1","name":"$name"}}}""" + "\n\n"
        )
        .mkString + "event: complete\n\n"
      for {
        endpoint <- sseEndpoint(body)
        reviews  <-
          stub(
            """{"data":{"_entities":[{"reviews":[{"body":"one"}]}]}}""",
            """{"data":{"_entities":[{"reviews":[{"body":"two"}]}]}}"""
          )
        runtime  <- Gateway
                      .compose(
                        Subgraph.federation("products", endpoint, products, sseConfig),
                        Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                      )
                      .interpreter
        events   <- runtime
                      .executeStream(GraphQLRequest(query = Some("subscription { changed { name reviews { body } } }")))
                      .runCollect
        sent     <- reviews.requests.get
      } yield assertTrue(
        events.size == 2,
        events.forall(_.errors.isEmpty),
        events.head.data.toString.contains("one"),
        events.last.data.toString.contains("two"),
        sent.size == 2
      )
    },
    test("upstream WebSocket terminal errors retain only the first disclosed error") {
      val terminal =
        """{"type":"error","id":"1","payload":[{"message":"secret first","path":["event"],"extensions":{"code":"FIRST","secret":true}},{"message":"secret second","path":["event"],"extensions":{"code":"SECOND"}}]}"""
      val socket   = transportWsSocket(channel =>
        acknowledgeThen(channel) { case "subscribe" => channel.send(ChannelEvent.Read(WebSocketFrame.Text(terminal))) }
      )
      for {
        sent     <- Ref.make(List.empty[String])
        endpoint <- getEndpoint("subscription-ws-error")(req =>
                      sent.set(renderedHeaderValues(req.headers, "X-Multi")).zipRight(Response.fromSocketApp(socket))
                    )
        config    = RemoteGraphQLConfig.default
                      .withExecutionHeadersZIO(
                        ZIO.succeed(
                          List(
                            Header.Custom("X-Multi", "first"),
                            Header.Custom("X-Multi", "second")
                          )
                        )
                      )
                      .withSubscription(_ => RemoteSubscriptionConfig.default)
        runtime  <- remoteGateway(endpoint, subscriptionSchema, config).interpreter
        exit     <- runtime.executeStream(request).runDrain.exit
        error     = exit.causeOption.flatMap(_.failureOption).collect { case e: CalibanError.ExecutionError => e }
        values   <- sent.get
      } yield assertTrue(
        error.exists(_.msg == "Remote GraphQL request failed."),
        error.exists(_.path == List(PathValue.Key("event"))),
        error.flatMap(_.extensions).contains(ResponseValue.ObjectValue(List("code" -> Value.StringValue("FIRST")))),
        values == List("first, second")
      )
    },
    test("modern upstream WebSocket streams through the public Quick adapter") {
      val api = subscriptionGraph(ZStream(1, 2))
      for {
        source   <- api.interpreter
        endpoint <- routesEndpoint("subscription-ws")(path =>
                      QuickAdapter(source).routes(s"/$path", webSocketPath = Some(s"/$path/ws"))
                    )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      _.withEndpoint(endpoint.addPath("ws"))
                    )
        runtime  <- remoteGateway(endpoint, api.render, config).interpreter
        events   <- runtime.executeStream(request).runCollect
      } yield assertTrue(
        events.map(_.data.toString).toList == List("{\"event\":1}", "{\"event\":2}")
      )
    },
    test("Quick uses multipart for either hasNext value even when SSE is accepted") {
      ZIO
        .foreach(List(true, false)) { hasNext =>
          val response    = GraphQLResponse(
            ResponseValue.StreamValue(ZStream.succeed(ResponseValue.ObjectValue(List("event" -> Value.IntValue(1))))),
            Nil,
            hasNext = Some(hasNext)
          )
          val interpreter = new GraphQLInterpreter[Any, CalibanError] {
            def check(query: String)(implicit trace: Trace)                    = ZIO.unit
            def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = ZIO.succeed(response)
          }
          for {
            result <- QuickAdapter(interpreter).handlers.api.runZIO(
                        Request
                          .post(
                            URL.empty,
                            Body.fromString("""{"query":"{ event }"}""").contentType(MediaType.application.json)
                          )
                          .addHeader(Header.Custom("Accept", "text/event-stream"))
                      )
            body   <- result.body.asString
          } yield assertTrue(
            result.headers.get(Header.ContentType).exists(_.mediaType.fullType == "multipart/mixed"),
            body.contains(s""""hasNext":$hasNext"""),
            !body.contains("event: next")
          )
        }
        .map(_.reduce(_ && _))
    },
    test("Quick SSE emits complete envelopes and a completion event") {
      for {
        runtime  <- subscriptionGateway(ZStream(1, 2)).interpreter
        response <- QuickAdapter(runtime).handlers.api
                      .runZIO(
                        Request
                          .post(
                            URL.empty,
                            Body
                              .fromString("""{"query":"subscription { event }"}""")
                              .contentType(MediaType.application.json)
                          )
                          .addHeader(Header.Custom("Accept", "text/event-stream"))
                      )
        body     <- response.body.asString
      } yield assertTrue(
        response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "text/event-stream"),
        body.contains("\"event\":1"),
        body.contains("\"event\":2"),
        body.contains("event: complete")
      )
    }
  ).provideSomeLayerShared[Scope](testServer ++ stubIds) @@ TestAspect.timeout(30.seconds) @@ TestAspect.sequential
}
