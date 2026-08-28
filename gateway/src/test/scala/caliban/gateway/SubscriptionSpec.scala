package caliban.gateway

import caliban._
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.{ GatewayExecutionControl, SubscriptionControl }
import caliban.gateway.internal.execution.SubgraphExecutor
import caliban.schema.Schema.auto._
import caliban.ws.{ Protocol, WebSocketHooks }
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import sttp.model.Uri
import zio._
import zio.http._
import zio.stream.ZStream
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

object SubscriptionSpec extends ZIOSpecDefault {
  final case class Query(value: String)
  final case class Subscription(event: ZStream[Any, Throwable, Int])
  private val request                                     = GraphQLRequest(query = Some("subscription { event }"))
  private def local(events: ZStream[Any, Throwable, Int]) = Subgraph.local(
    "local",
    graphQL(
      RootResolver(
        queryResolver = Some(Query("ok")),
        mutationResolver = Option.empty[Unit],
        subscriptionResolver = Some(Subscription(events))
      )
    )
  )

  def spec = suite("SubscriptionSpec")(
    test("source event size is bounded before buffering and federation processing") {
      for {
        processed <- Ref.make(0)
        work      <- GatewayExecutionControl.make(1, Map.empty, 30.seconds, 1.second)
        control   <-
          SubscriptionControl.make(GatewaySubscriptionConfig(maxEventBytes = 64), 1.second, work, GatewayWrapper.empty)
        oversized  = GraphQLResponse(Value.StringValue("x" * 128), Nil)
        exit      <- control
                       .stream(None)(ZIO.succeed(ZStream.succeed(oversized))) { _ =>
                         processed.update(_ + 1).as(GraphQLResponse(Value.NullValue, Nil))
                       }
                       .runCollect
                       .exit
        count     <- processed.get
        status    <- control.status
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge),
        count == 0,
        status.active == 0
      )
    },
    test("envelope errors retain public fields while terminal failures retain identity") {
      val failure  = SubscriptionTermination.Reload
      val event    = GraphQLResponse(Value.NullValue, List(failure))
      val response = GraphQLResponse(
        ResponseValue.StreamValue(ZStream.succeed(event.toResponseValue) ++ ZStream.fail(failure)),
        Nil
      )
      SubgraphExecutor.responses(response).either.runCollect.map { values =>
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
          SubgraphExecutor.responses(response).runCollect.map(values => assertTrue(values == Chunk(response)))
        }
        .map(_.reduce(_ && _))
    },
    test("failed or cancelled setup reports terminating and overdue while finalizers finish") {
      ZIO
        .foreach(List(false, true)) { cancel =>
          for {
            opened     <- Promise.make[Nothing, Unit]
            closing    <- Promise.make[Nothing, Unit]
            release    <- Promise.make[Nothing, Unit]
            work       <- GatewayExecutionControl.make(1, Map.empty, 30.seconds, 1.second)
            control    <- SubscriptionControl
                            .make(GatewaySubscriptionConfig(), 1.second, work, GatewayWrapper.empty)
            open        = ZIO.addFinalizer(closing.succeed(()) *> release.await) *>
                            opened.succeed(()) *> (if (cancel) ZIO.never else ZIO.fail(SubscriptionTermination.Source))
            running    <- control.stream(None)(open)(ZIO.succeed(_)).runDrain.exit.forkScoped
            _          <- opened.await
            cancelling <- (if (cancel) running.interrupt.unit else ZIO.unit).forkScoped
            _          <- closing.await
            during     <- control.status
            _          <- TestClock.adjust(2.seconds)
            overdue    <- control.status
            _          <- release.succeed(())
            _          <- cancelling.join
            _          <- running.await
            after      <- control.status
          } yield assertTrue(during.establishing == 0, during.terminating == 1, overdue.overdue == 1, after.active == 0)
        }
        .map(_.reduce(_ && _))
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
              .as(
                Response(
                  headers = Headers(Header.Custom("Content-Type", "text/event-stream")),
                  body = Body.fromString("event: next\ndata: {\"data\":{\"event\":1}}\n\nevent: complete\n\n")
                )
              )
          )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
                    )
        gateway  <- Gateway
                      .compose(Subgraph.graphql("remote", endpoint, schema, config))
                      .withOperationResolver(OperationResolver(_ => ZIO.succeed(query)))
                      .interpreter
        events   <-
          gateway
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
            endpoint <- streamingEndpoint(ZStream.fromIterable(body.getBytes(UTF_8)), mediaType = "text/event-stream")
            config    = RemoteGraphQLConfig.default
                          .withExecution(_.withMaxResponseBytes(128))
                          .withSubscription(RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse()))
            gateway  <- Gateway
                          .compose(
                            Subgraph.graphql(
                              "remote",
                              endpoint,
                              "type Query { value: String } type Subscription { event: Int }",
                              config
                            )
                          )
                          .interpreter
            events   <- gateway.executeStream(request).runCollect
            status   <- gateway.subscriptionStatus
          } yield assertTrue(events.map(_.data.toString).toList == List("{\"event\":1}"), status.active == 0)
        }
        .map(_.reduce(_ && _))
    },
    test("SSE comments do not reset the accumulated data limit") {
      val body = "event: next\n" + (("data: " + ("x" * 40) + "\n: heartbeat\n") * 4)
      for {
        endpoint <- streamingEndpoint(ZStream.fromIterable(body.getBytes(UTF_8)), mediaType = "text/event-stream")
        config    = RemoteGraphQLConfig.default
                      .withExecution(_.withMaxResponseBytes(128))
                      .withSubscription(RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse()))
        gateway  <-
          Gateway
            .compose(
              Subgraph
                .graphql("remote", endpoint, "type Query { value: String } type Subscription { event: Int }", config)
            )
            .interpreter
        exit     <- gateway.executeStream(request).runCollect.exit
        status   <- gateway.subscriptionStatus
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge),
        status.active == 0
      )
    },
    test("remote stream size limits preserve gateway termination codes") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      for {
        input    <- Queue.unbounded[String]
        first    <- Promise.make[Nothing, Unit]
        endpoint <- streamingEndpoint(
                      ZStream
                        .fromQueue(input)
                        .flatMap(text => ZStream.fromIterable(text.getBytes(UTF_8))),
                      mediaType = "text/event-stream"
                    )
        config    = RemoteGraphQLConfig.default
                      .withExecution(_.withMaxResponseBytes(128))
                      .withSubscription(RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse()))
        gateway  <- Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)).interpreter
        running  <- gateway.executeStream(request).tap(_ => first.succeed(())).runDrain.exit.forkScoped
        _        <- input.offer("event: next\ndata: {\"data\":{\"event\":1}}\n\n")
        _        <- first.await
        _        <- input.offer(":" + ("x" * 129))
        exit     <- running.join
      } yield assertTrue(exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge))
    },
    test("upstream buffer overflow keeps its code and emits the overflow observation") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      for {
        closed       <- Promise.make[Nothing, Unit]
        release      <- Promise.make[Nothing, Unit]
        seen         <- Ref.make(List.empty[GatewayWrapper.Event])
        socket        = Handler
                          .webSocket(channel =>
                            ZIO.scoped {
                              channel.awaitShutdown.ensuring(closed.succeed(())).forkScoped *>
                                channel.receiveAll {
                                  case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
                                    readFromString[GraphQLWSInput](text).`type` match {
                                      case "connection_init" =>
                                        channel.send(ChannelEvent.Read(WebSocketFrame.Text("""{"type":"connection_ack"}""")))
                                      case "subscribe"       =>
                                        ZIO.foreachDiscard(1 to 2)(value =>
                                          channel.send(
                                            ChannelEvent.Read(
                                              WebSocketFrame.Text(
                                                s"""{"type":"next","id":"1","payload":{"data":{"event":$value}}}"""
                                              )
                                            )
                                          )
                                        )
                                      case _                 => ZIO.unit
                                    }
                                  case _                                            => ZIO.unit
                                }
                            }
                          )
                          .withConfig(WebSocketConfig.default.subProtocol(Some("graphql-transport-ws")))
        id           <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path          = s"subscription-ws-overflow-$id"
        server       <- ZIO.service[Server]
        _            <- server.install(
                          Routes(Method.GET / path -> Handler.fromFunctionZIO[Request](_ => Response.fromSocketApp(socket)))
                        )
        port         <- server.port
        endpoint      = Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
        wrapper       = new GatewayWrapper[Any] {
                          def wrap[R, E, A](event: GatewayWrapper.Event)(effect: ZIO[R, E, A])(
                            result: Exit[E, A] => GatewayWrapper.Result
                          )(implicit trace: Trace): ZIO[R, E, A] =
                            seen.update(event :: _) *> effect.flatMap(value =>
                              (if (event == GatewayWrapper.Event.SubscriptionSetup) release.await else ZIO.unit).as(value)
                            )
                        }
        config        = RemoteGraphQLConfig.default.withSubscription(RemoteSubscriptionConfig(bufferSize = 1))
        gateway      <- (Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)) @@ wrapper).interpreter
        running      <- gateway.executeStream(request).runDrain.exit.forkScoped
        _            <- closed.await
        _            <- release.succeed(())
        exit         <- running.join
        observations <- seen.get
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Overflow),
        observations.count(_ == GatewayWrapper.Event.SubscriptionOverflow) == 1,
        observations.collect { case GatewayWrapper.Event.SubscriptionTerminated(reason, _) => reason } == List(
          "SUBSCRIPTION_OVERFLOW"
        )
      )
    },
    test("pong timeout is independent of the longer keepalive interval") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      for {
        first   <- Promise.make[Nothing, Unit]
        ping    <- Promise.make[Nothing, Unit]
        socket   = Handler
                     .webSocket(channel =>
                       channel.receiveAll {
                         case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
                           readFromString[GraphQLWSInput](text).`type` match {
                             case "connection_init" =>
                               channel.send(ChannelEvent.Read(WebSocketFrame.Text("""{"type":"connection_ack"}""")))
                             case "subscribe"       =>
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
                             case "ping"            => ping.succeed(()).unit
                             case _                 => ZIO.unit
                           }
                         case _                                            => ZIO.unit
                       }
                     )
                     .withConfig(WebSocketConfig.default.subProtocol(Some("graphql-transport-ws")))
        id      <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path     = s"subscription-ws-pong-$id"
        server  <- ZIO.service[Server]
        _       <- server.install(
                     Routes(Method.GET / path -> Handler.fromFunctionZIO[Request](_ => Response.fromSocketApp(socket)))
                   )
        port    <- server.port
        endpoint = Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
        config   = RemoteGraphQLConfig.default.withSubscription(
                     RemoteSubscriptionConfig(keepAliveInterval = 60.seconds, pongTimeout = 1.second)
                   )
        gateway <- Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)).interpreter
        running <- gateway.executeStream(request).tap(_ => first.succeed(())).runDrain.forkScoped
        _       <- first.await
        _       <- TestClock.adjust(60.seconds)
        _       <- ping.await
        _       <- TestClock.adjust(2.seconds)
        exit    <- Live.live(running.await.timeout(1.second))
        status  <- gateway.subscriptionStatus
        _       <- running.interrupt
      } yield assertTrue(
        exit.flatMap(_.causeOption).flatMap(_.failureOption).contains(SubscriptionTermination.Source),
        status.active == 0
      )
    },
    test("resolved subscriptions use executeRequest without entering finite-request metrics") {
      for {
        seen     <- Ref.make(Vector.empty[GatewayWrapper.Event])
        resolves <- Ref.make(0)
        wrapper   = new GatewayWrapper[Any] {
                      def wrap[R, E, A](event: GatewayWrapper.Event)(effect: ZIO[R, E, A])(
                        result: Exit[E, A] => GatewayWrapper.Result
                      )(implicit trace: Trace): ZIO[R, E, A] = seen.update(_ :+ event) *> effect
                    }
        gateway  <- (Gateway
                      .compose(local(ZStream(1, 2)))
                      .withOperationResolver(
                        OperationResolver(_ => resolves.update(_ + 1).as("subscription { event }"))
                      ) @@ wrapper).interpreter
        response <- gateway.executeRequest(GraphQLRequest(query = Some("query { value }")))
        before   <- gateway.subscriptionStatus
        events   <- SubgraphExecutor.responses(response).runCollect
        recorded <- seen.get
        count    <- resolves.get
      } yield assertTrue(
        response.data.isInstanceOf[ResponseValue.StreamValue],
        before.active == 0,
        events.map(_.data.toString).toList == List("{\"event\":1}", "{\"event\":2}"),
        !recorded.exists(_.isInstanceOf[GatewayWrapper.Event.Request]),
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
        gateway  <- owner.extend(Gateway.compose(local(source)).withConfig(_.withDrainTimeout(1.second)).interpreter)
        running  <- gateway.executeStream(request).runDrain.exit.forkScoped
        _        <- opened.await
        stopping <- owner.close(Exit.unit).forkScoped
        _        <- closing.await
        _        <- TestClock.adjust(2.seconds)
        status   <- gateway.status.repeatUntil(_.lifecycle.state != GatewayInterpreter.LifecycleState.Running)
        rejected <- gateway.execute("{ value }")
        slots    <- gateway.subscriptionStatus
        pending  <- stopping.poll
        _        <- release.succeed(())
        _        <- stopping.join
        _        <- running.join
        after    <- gateway.status
      } yield assertTrue(
        status.lifecycle.state == GatewayInterpreter.LifecycleState.Draining,
        rejected.errors.nonEmpty,
        slots.active == 1,
        slots.overdue == 1,
        pending.isEmpty,
        after.lifecycle.state == GatewayInterpreter.LifecycleState.Closed
      )
    },
    test("SSE GET supports BOM, CR line endings, and a distinct completion event") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      val body   = "\uFEFF: heartbeat\r\revent: next\rdata: {\"data\":{\"event\":1}}\r\revent: complete\r\r"
      for {
        sent    <- Ref.make(Option.empty[String])
        id      <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path     = s"subscription-get-$id"
        server  <- ZIO.service[Server]
        handler  = Handler.fromFunctionZIO[Request](req =>
                     sent
                       .set(req.url.queryParams.getAll("query").headOption)
                       .as(
                         Response(
                           headers = Headers(Header.Custom("Content-Type", "text/event-stream")),
                           body = Body.fromString(body)
                         )
                       )
                   )
        _       <- server.install(Routes(Method.GET / path -> handler))
        port    <- server.port
        endpoint = Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
        config   = RemoteGraphQLConfig.default.withSubscription(
                     RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse(useGet = true))
                   )
        gateway <- Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)).interpreter
        events  <- gateway.executeStream(request).runCollect
        query   <- sent.get
      } yield assertTrue(
        events.map(_.data.toString).toList == List("{\"event\":1}"),
        query.exists(_.startsWith("subscription"))
      )
    },
    test("source completion drains every buffered event in order") {
      val values = (1 to 100).toList
      for {
        gateway <- Gateway
                     .compose(local(ZStream.fromIterable(values)))
                     .withConfig(_.withSubscriptions(GatewaySubscriptionConfig(bufferSize = 128)))
                     .interpreter
        events  <- gateway.executeStream(request).runCollect
        slots   <- gateway.subscriptionStatus
      } yield assertTrue(events.map(_.data.toString).toList == values.map(i => s"""{"event":$i}"""), slots.active == 0)
    },
    test("two subscriptions on one socket hold and release independent slots") {
      for {
        closed      <- Ref.make(0)
        source       = (ZStream.succeed(1) ++ ZStream.never).ensuring(closed.update(_ + 1))
        gateway     <- Gateway.compose(local(source)).interpreter
        input       <- Queue.unbounded[GraphQLWSInput]
        output      <- Queue.unbounded[Either[GraphQLWSClose, GraphQLWSOutput]]
        pipe        <-
          Protocol.GraphQLWS
            .make(gateway, None, WebSocketHooks.empty[Any, CalibanError])
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
        both        <- gateway.subscriptionStatus
        _           <- input.offer(GraphQLWSInput("complete", Some("1"), None))
        one         <- gateway.subscriptionStatus.repeatUntil(_.active == 1)
        firstClosed <- closed.get
        _           <- input.offer(GraphQLWSInput("complete", Some("2"), None))
        none        <- gateway.subscriptionStatus.repeatUntil(_.active == 0)
        allClosed   <- closed.get
        _           <- socket.interrupt
      } yield assertTrue(both.active == 2, one.active == 1, firstClosed == 1, none.active == 0, allClosed == 2)
    },
    test("captures effectful headers during setup and evaluates policy only once") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      val body   =
        "event: next\ndata: {\"data\":{\"event\":1}}\n\nevent: next\ndata: {\"data\":{\"event\":2}}\n\nevent: complete\n\n"
      for {
        identity    <- FiberRef.make("later")
        headers     <- Ref.make(List.empty[String])
        policies    <- Ref.make(0)
        headerCalls <- Ref.make(0)
        endpoint    <- postEndpoint("subscription-identity")(req =>
                         headers
                           .update(_ ++ req.headers.get("X-Identity").toList)
                           .as(
                             Response(
                               headers = Headers(Header.Custom("Content-Type", "text/event-stream")),
                               body = Body.fromString(body)
                             )
                           )
                       )
        config       =
          RemoteGraphQLConfig.default
            .withSubscription(RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse()))
            .withExecutionHeadersZIO(
              headerCalls.update(_ + 1) *> identity.get.map(value => List(sttp.model.Header("X-Identity", value)))
            )
        gateway     <- Gateway
                         .compose(Subgraph.graphql("remote", endpoint, schema, config))
                         .withOperationPolicy(OperationPolicy[Any](_ => policies.update(_ + 1).as(OperationPolicy.Allow)))
                         .interpreter
        events      <- identity.locally("captured")(gateway.executeStream(request).runCollect)
        sent        <- headers.get
        calls       <- headerCalls.get
        checks      <- policies.get
      } yield assertTrue(events.size == 2, sent == List("captured"), calls == 1, checks == 1)
    },
    test("idle lifetime ignores request timeout and supplied expiry terminates the source") {
      for {
        opened  <- Promise.make[Nothing, Unit]
        gateway <- Gateway
                     .compose(local(ZStream.fromZIO(opened.succeed(())) *> ZStream.never))
                     .withConfig(_.withRequestTimeout(1.second))
                     .interpreter
        now     <- Clock.instant
        running <-
          SubscriptionIdentity.withExpiry(now.plusSeconds(10))(gateway.executeStream(request).runDrain.exit).forkScoped
        _       <- opened.await
        _       <- TestClock.adjust(2.seconds)
        idle    <- running.poll
        status  <- gateway.status
        _       <- TestClock.adjust(8.seconds)
        exit    <- running.join
        slots   <- gateway.subscriptionStatus
      } yield assertTrue(
        idle.isEmpty,
        status.requests.active == 0,
        slots.active == 0,
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Expired)
      )
    },
    test("ordinary interpreter middleware preserves subscriptions and its response transformation") {
      for {
        opened   <- Ref.make(0)
        gateway  <- Gateway.compose(local(ZStream.fromZIO(opened.updateAndGet(_ + 1)))).interpreter
        wrapped   =
          gateway.wrapExecutionWith(
            _.map(_.copy(extensions = Some(ResponseValue.ObjectValue(List("wrapped" -> Value.BooleanValue(true))))))
          )
        response <- wrapped.executeRequest(request)
        count    <- opened.get
      } yield assertTrue(
        response.errors.isEmpty,
        response.data.isInstanceOf[ResponseValue.StreamValue],
        response.extensions.nonEmpty,
        count == 0
      )
    },
    test("event size limits terminate without delivering an oversized event") {
      for {
        gateway <- Gateway
                     .compose(local(ZStream(1)))
                     .withConfig(_.withSubscriptions(GatewaySubscriptionConfig(maxEventBytes = 1)))
                     .interpreter
        exit    <- gateway.executeStream(request).runCollect.exit
        status  <- gateway.subscriptionStatus
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.TooLarge),
        status.active == 0
      )
    },
    test("overflow sheds the operation and records a separate termination") {
      for {
        queue      <- Queue.unbounded[Int]
        processing <- Promise.make[Nothing, Unit]
        seen       <- Ref.make(List.empty[GatewayWrapper.Event])
        wrapper     = new GatewayWrapper[Any] {
                        def wrap[R, E, A](event: GatewayWrapper.Event)(
                          effect: ZIO[R, E, A]
                        )(result: Exit[E, A] => GatewayWrapper.Result)(implicit trace: Trace): ZIO[R, E, A] =
                          seen.update(event :: _) *> (if (event == GatewayWrapper.Event.SubscriptionEvent)
                                                        processing.succeed(()) *> ZIO.never
                                                      else effect)
                      }
        gateway    <- (Gateway
                        .compose(local(ZStream.fromQueue(queue)))
                        .withConfig(_.withSubscriptions(GatewaySubscriptionConfig(bufferSize = 1))) @@ wrapper).interpreter
        running    <- gateway.executeStream(request).runDrain.exit.forkScoped
        _          <- queue.offer(1)
        _          <- processing.await
        _          <- queue.offerAll(List(2, 3, 4))
        exit       <- running.join
        events     <- seen.get
        slots      <- gateway.subscriptionStatus
      } yield assertTrue(
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Overflow),
        slots.active == 0,
        events.contains(GatewayWrapper.Event.SubscriptionOverflow),
        events.collect { case GatewayWrapper.Event.SubscriptionTerminated(reason, _) => reason } == List(
          "SUBSCRIPTION_OVERFLOW"
        ),
        !events.exists(_.isInstanceOf[GatewayWrapper.Event.Request])
      )
    },
    test("local events are ordered, planned once, and executeRequest returns an ordinary StreamValue") {
      for {
        gateway <- Gateway.compose(local(ZStream(1, 2, 3))).interpreter
        plan    <- gateway.explain(request)
        events  <- gateway.executeStream(request).runCollect
        status  <- gateway.status
        slots   <- gateway.subscriptionStatus
        finite  <- gateway.executeRequest(request)
      } yield assertTrue(
        plan.nonEmpty,
        events.map(_.data.toString).toList == List("{\"event\":1}", "{\"event\":2}", "{\"event\":3}"),
        events.forall(_.errors.isEmpty),
        status.operationCache.entries == 1,
        status.operationCache.misses == 1,
        slots.active == 0,
        finite.errors.isEmpty,
        finite.data.isInstanceOf[ResponseValue.StreamValue],
        finite.hasNext.isEmpty
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
        native   <- SubgraphExecutor.responses(response).runCollect
        gateway  <- Gateway.compose(Subgraph.local("local", api)).interpreter
        events   <- gateway.executeStream(request).runCollect
      } yield assertTrue(
        events == native,
        events.size == 2,
        events.head.data.toString == "{\"event\":{\"value\":null}}",
        events.last.data.toString == "{\"event\":{\"value\":\"next\"}}",
        events.forall(_.errors.isEmpty)
      )
    },
    test("idle subscription holds one slot and no finite permits; cancellation awaits source cleanup") {
      for {
        opened   <- Promise.make[Nothing, Unit]
        closing  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        source    =
          ZStream.acquireReleaseWith(opened.succeed(()))(_ => closing.succeed(()) *> release.await) *> ZStream.never
        gateway  <-
          Gateway
            .compose(local(source))
            .withConfig(_.withMaxConcurrentRequests(1).withSubscriptions(GatewaySubscriptionConfig(maxActive = 1)))
            .interpreter
        running  <- gateway.executeStream(request).runDrain.forkScoped
        _        <- opened.await
        status   <- gateway.status
        slots    <- gateway.subscriptionStatus
        rejected <- gateway.executeStream(request).runDrain.exit
        query    <- gateway.execute("{ value }")
        stopping <- running.interrupt.forkScoped
        _        <- closing.await
        during   <- gateway.subscriptionStatus
        _        <- release.succeed(())
        _        <- stopping.join
        after    <- gateway.subscriptionStatus
      } yield assertTrue(
        status.requests.active == 0,
        status.subgraphs.values.forall(_.active == 0),
        slots.active == 1,
        rejected.isFailure,
        query.errors.isEmpty,
        during.active == 1,
        after.active == 0
      )
    },
    test("constructing a stream acquires nothing and a closed gateway rejects consumption") {
      for {
        opens        <- Ref.make(0)
        gatewayScope <- Scope.make
        gateway      <- gatewayScope.extend(Gateway.compose(local(ZStream.fromZIO(opens.updateAndGet(_ + 1)))).interpreter)
        result        = gateway.executeStream(request)
        before       <- gateway.status
        slots        <- gateway.subscriptionStatus
        _            <- gatewayScope.close(Exit.unit)
        responses    <- result.runCollect
        count        <- opens.get
      } yield assertTrue(
        before.operationCache.entries == 0,
        slots.active == 0,
        responses.head.errors.nonEmpty,
        count == 0
      )
    },
    test("expired identity cannot lazily acquire the source") {
      for {
        opens   <- Ref.make(0)
        gateway <- Gateway.compose(local(ZStream.fromZIO(opens.updateAndGet(_ + 1)))).interpreter
        now     <- Clock.instant
        result  <- SubscriptionIdentity.withExpiry(now.plusSeconds(1))(gateway.executeRequest(request))
        _       <- TestClock.adjust(2.seconds)
        exit    <- SubgraphExecutor.responses(result).runDrain.exit
        count   <- opens.get
      } yield assertTrue(exit.isFailure, count == 0)
    },
    test("SSE source preserves data and redacts errors and response extensions") {
      val schema = "type Query { value: String } type Subscription { event: Int }"
      val body   =
        "event: next\ndata: {\"data\":{\"event\":1},\"errors\":[{\"message\":\"secret\",\"path\":[\"event\"],\"extensions\":{\"secret\":true}},{\"message\":\"second secret\",\"path\":[\"event\"]}],\"extensions\":{\"secret\":true}}\n\nevent: complete\n\n"
      for {
        endpoint <- streamingEndpoint(
                      ZStream.fromIterable(body.getBytes(UTF_8)),
                      mediaType = "text/event-stream"
                    )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
                    )
        gateway  <- Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)).interpreter
        events   <- gateway.executeStream(request).runCollect
      } yield assertTrue(
        events.size == 1,
        events.head.errors.size == 2,
        !events.head.toString.contains("secret"),
        events.head.extensions.isEmpty
      )
    },
    test("plans once and hydrates every source event with fresh entity results") {
      val products =
        productsFederationSchema.replace("{ query: Query }", "{ query: Query subscription: Subscription }") +
          " type Subscription { changed: Product }"
      val body     = List("first", "second")
        .map(name =>
          "event: next\ndata: " + s"""{"data":{"changed":{"_caliban_gateway_typename":"Product","_caliban_gateway_key":"1","name":"$name"}}}""" + "\n\n"
        )
        .mkString + "event: complete\n\n"
      for {
        endpoint <- streamingEndpoint(
                      ZStream.fromIterable(body.getBytes(UTF_8)),
                      mediaType = "text/event-stream"
                    )
        reviews  <-
          stub(
            """{"data":{"_entities":[{"reviews":[{"body":"one"}],"_caliban_gateway_entity_key":"1","_caliban_gateway_entity_typename":"Product"}]}}""",
            """{"data":{"_entities":[{"reviews":[{"body":"two"}],"_caliban_gateway_entity_key":"1","_caliban_gateway_entity_typename":"Product"}]}}"""
          )
        config    = RemoteGraphQLConfig.default.withSubscription(
                      RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
                    )
        gateway  <- Gateway
                      .compose(
                        Subgraph.federation("products", endpoint, products, config),
                        Subgraph.federation("reviews", reviews.endpoint, reviewsFederationSchema)
                      )
                      .interpreter
        events   <- gateway
                      .executeStream(GraphQLRequest(query = Some("subscription { changed { name reviews { body } } }")))
                      .runCollect
        sent     <- reviews.requests.get
        status   <- gateway.status
      } yield assertTrue(
        events.size == 2,
        events.forall(_.errors.isEmpty),
        events.head.data.toString.contains("one"),
        events.last.data.toString.contains("two"),
        sent.size == 2,
        status.operationCache.misses == 1
      )
    },
    test("upstream WebSocket terminal errors retain only the first disclosed error") {
      val schema   = "type Query { value: String } type Subscription { event: Int }"
      val terminal =
        """{"type":"error","id":"1","payload":[{"message":"secret first","path":["event"],"extensions":{"code":"FIRST","secret":true}},{"message":"secret second","path":["event"],"extensions":{"code":"SECOND"}}]}"""
      val socket   = Handler
        .webSocket(channel =>
          channel.receiveAll {
            case ChannelEvent.Read(WebSocketFrame.Text(text)) =>
              val message = readFromString[GraphQLWSInput](text)
              message.`type` match {
                case "connection_init" =>
                  channel.send(ChannelEvent.Read(WebSocketFrame.Text("""{"type":"connection_ack"}""")))
                case "subscribe"       => channel.send(ChannelEvent.Read(WebSocketFrame.Text(terminal)))
                case _                 => ZIO.unit
              }
            case _                                            => ZIO.unit
          }
        )
        .withConfig(WebSocketConfig.default.subProtocol(Some("graphql-transport-ws")))
      for {
        id      <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path     = s"subscription-ws-error-$id"
        server  <- ZIO.service[Server]
        handler  = Handler.fromFunctionZIO[Request](_ => Response.fromSocketApp(socket))
        _       <- server.install(Routes(Method.GET / path -> handler))
        port    <- server.port
        endpoint = Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
        config   = RemoteGraphQLConfig.default.withSubscription(RemoteSubscriptionConfig())
        gateway <- Gateway.compose(Subgraph.graphql("remote", endpoint, schema, config)).interpreter
        exit    <- gateway.executeStream(request).runDrain.exit
        slots   <- gateway.subscriptionStatus
        error    = exit.causeOption.flatMap(_.failureOption).collect { case e: CalibanError.ExecutionError => e }
      } yield assertTrue(
        error.exists(_.msg == "Remote GraphQL request failed."),
        error.exists(_.path == List(PathValue.Key("event"))),
        error.flatMap(_.extensions).contains(ResponseValue.ObjectValue(List("code" -> Value.StringValue("FIRST")))),
        slots.active == 0
      )
    },
    test("modern upstream WebSocket streams through the public Quick adapter") {
      val api = graphQL(
        RootResolver(
          queryResolver = Some(Query("ok")),
          mutationResolver = Option.empty[Unit],
          subscriptionResolver = Some(Subscription(ZStream(1, 2)))
        )
      )
      for {
        source  <- api.interpreter
        id      <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
        path     = s"subscription-ws-$id"
        server  <- ZIO.service[Server]
        _       <- server.install(QuickAdapter(source).routes(s"/$path", webSocketPath = Some(s"/$path/ws")))
        port    <- server.port
        endpoint = Uri.unsafeParse(s"http://127.0.0.1:$port/$path")
        config   = RemoteGraphQLConfig.default.withSubscription(
                     RemoteSubscriptionConfig(endpoint = Some(endpoint.addPath("ws")))
                   )
        gateway <- Gateway.compose(Subgraph.graphql("remote", endpoint, api.render, config)).interpreter
        events  <- gateway.executeStream(request).runCollect
        slots   <- gateway.subscriptionStatus
      } yield assertTrue(
        events.map(_.data.toString).toList == List("{\"event\":1}", "{\"event\":2}"),
        slots.active == 0
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
        gateway  <- Gateway.compose(local(ZStream(1, 2))).interpreter
        response <- QuickAdapter(gateway).handlers.api
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
        slots    <- gateway.subscriptionStatus
      } yield assertTrue(
        response.headers.get(Header.ContentType).exists(_.mediaType.fullType == "text/event-stream"),
        body.contains("\"event\":1"),
        body.contains("\"event\":2"),
        body.contains("event: complete"),
        slots.active == 0
      )
    }
  ).provideSomeLayerShared[Scope](testServer ++ stubIds) @@ TestAspect.timeout(30.seconds) @@ TestAspect.sequential
}
