package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.PhaseHooks.Rejection
import caliban.gateway.internal.OperationPreparation
import caliban.{ CalibanError, GraphQLRequest, InputValue }
import caliban.Value.StringValue
import zio._
import zio.test._

object OperationResolutionSpec extends ZIOSpecDefault {

  private val query   = "query Value($input: String) { value(input: $input) }"
  private val request = GraphQLRequest(
    operationName = Some("Value"),
    variables = Some(Map("input" -> StringValue("hello"))),
    extensions = Some(Map("documentId" -> StringValue("value-v1")))
  )

  private def documentId(request: GraphQLRequest): Option[String] =
    request.extensions.flatMap(_.get("documentId")).collect { case StringValue(id) => id }

  def spec = suite("OperationResolutionSpec")(
    test("composes resolution handlers and prepares the transformed request") {
      for {
        remote   <- stub(okResponse)
        seen     <- Ref.make(List.empty[String])
        handler   =
          (name: String, query: String) =>
            PhaseHooks.resolutionHandler(PhaseHandler[Any, PhaseHooks.Event.Resolution, Throwable, Unit, Any] { event =>
              seen
                .update(_ :+ s"$name-in:${event.request.query.getOrElse("")}")
                .as(
                  (event.copy(request = event.request.copy(query = Some(query))), ())
                )
            } { (event, _, _) =>
              seen.update(_ :+ s"$name-out:${event.request.query.getOrElse("")}")
            })
        runtime  <- remoteGateway(remote.endpoint)
                      .withPhaseHooks(handler("first", "intermediate"))
                      .withPhaseHooks(handler("second", query))
                      .interpreter
        response <- runtime.executeRequest(request)
        observed <- seen.get
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.isEmpty,
        observed == List("first-in:", "second-in:intermediate", s"second-out:$query", "first-out:intermediate"),
        sent.map(_.query) == Vector(Some(query)),
        sent.map(_.variables) == Vector(request.variables)
      )
    },
    test("trusted documents override client text and preserve request fields, including on cache hits") {
      for {
        remote   <- stub(okResponse)
        seen     <- Ref.make(List.empty[GraphQLRequest])
        runtime  <- remoteGateway(remote.endpoint)
                      .withPhaseHooks(PhaseHooks.trustedDocuments(Map("value-v1" -> query))(documentId))
                      .withPhaseHooks(
                        PhaseHooks.authorization[Any](operation => seen.update(_ :+ operation.request).unit)
                      )
                      .interpreter
        first    <- runtime.executeRequest(request)
        next      = request.copy(query = Some("{ unregistered }"), variables = Some(Map("input" -> StringValue("next"))))
        second   <- runtime.executeRequest(next)
        observed <- seen.get
        sent     <- remote.requests.get
      } yield assertTrue(
        first.errors.isEmpty,
        second.errors.isEmpty,
        observed == List(request.copy(query = Some(query)), next.copy(query = Some(query))),
        sent.map(_.query) == Vector(Some(query), Some(query)),
        sent.map(_.operationName) == Vector(request.operationName, next.operationName),
        sent.map(_.variables) == Vector(request.variables, next.variables)
      )
    },
    test("rejects missing, malformed, empty, and unknown IDs without falling back to supplied text") {
      val invalid = List(
        request.copy(extensions = None),
        request.copy(extensions = Some(Map.empty)),
        request.copy(extensions = Some(Map("documentId" -> InputValue.ObjectValue(Map.empty)))),
        request.copy(extensions = Some(Map("documentId" -> StringValue(""))))
      )
      val unknown = request.copy(extensions = Some(Map("documentId" -> StringValue("private-unknown-id"))))

      for {
        remote      <- stub(okResponse)
        policyCalls <- Ref.make(0)
        runtime     <-
          remoteGateway(remote.endpoint)
            .withPhaseHooks(PhaseHooks.trustedDocuments(Map("value-v1" -> query))(documentId))
            .withPhaseHooks(
              PhaseHooks.authorization[Any](_ => policyCalls.update(_ + 1).unit)
            )
            .interpreter
        rejected    <- ZIO.foreach(invalid)(r => runtime.executeRequest(r.copy(query = Some(query))))
        missing     <- runtime.executeRequest(unknown.copy(query = Some(query)))
        sent        <- remote.requests.get
        calls       <- policyCalls.get
      } yield assertTrue(
        rejected.forall(_.errors.map(_.msg) == List("A non-empty trusted document ID is required.")),
        rejected.forall(_.errors.flatMap(codeOf) == List("TRUSTED_DOCUMENT_ID_INVALID")),
        missing.errors.map(_.msg) == List("Trusted document not found."),
        missing.errors.flatMap(codeOf) == List("TRUSTED_DOCUMENT_NOT_FOUND"),
        sent.isEmpty,
        calls == 0
      )
    },
    test("uses the caller's extraction format and keeps IDs opaque") {
      val resolver                         = PhaseHooks.trustedDocuments(Map(" opaque ID " -> query))(_.operationName)
      def resolve(request: GraphQLRequest) =
        resolver.resolution.runWith(PhaseHooks.Event.Resolution(request))(event => ZIO.succeed(event.request))(_ => ())
      for {
        resolved <- resolve(GraphQLRequest(operationName = Some(" opaque ID ")))
        rejected <- resolve(GraphQLRequest(operationName = Some("opaque ID"))).either
      } yield assertTrue(
        resolved.query.contains(query),
        rejected == Left(Rejection("Trusted document not found.", "TRUSTED_DOCUMENT_NOT_FOUND"))
      )
    },
    test("resolves explain requests while check still validates literal text") {
      for {
        remote   <- stub(okResponse)
        runtime  <- remoteGateway(remote.endpoint)
                      .withPhaseHooks(PhaseHooks.trustedDocuments(Map("value-v1" -> query))(documentId))
                      .interpreter
        plan     <- runtime.explain(request)
        rejected <- runtime.explain(request.copy(extensions = None)).either
        checked  <- runtime.check(query).exit
        invalid  <- runtime.check("{ missing }").exit
        sent     <- remote.requests.get
      } yield assertTrue(
        plan.contains("remote"),
        rejected.left.toOption.flatMap(codeOf).contains("TRUSTED_DOCUMENT_ID_INVALID"),
        checked.isSuccess,
        invalid.isFailure,
        sent.isEmpty
      )
    },
    test("resolves every request before cache lookup and only uncached bypasses preparation reuse") {
      ZIO
        .foreach(List(false, true)) { uncached =>
          for {
            remote         <- stub(okResponse)
            recorded       <- recordEvents
            (events, hooks) = recorded
            calls          <- Ref.make(0)
            resolve         = (_: GraphQLRequest) =>
                                calls.updateAndGet(_ + 1).flatMap {
                                  case 1 | 2 => ZIO.succeed(query)
                                  case _     => ZIO.fail(Rejection("Revoked.", "REVOKED"))
                                }
            resolver        = PhaseHooks.resolution(resolve, cacheable = !uncached) ++
                                PhaseHooks.resolution[Any](request => ZIO.succeed(request.query.getOrElse("")))
            runtime        <- remoteGateway(remote.endpoint)
                                .withPhaseHooks(resolver)
                                .withPhaseHooks(hooks)
                                .interpreter
            first          <- runtime.executeRequest(request)
            second         <- runtime.executeRequest(request.copy(extensions = Some(Map("documentId" -> StringValue("alias")))))
            rejected       <- runtime.executeRequest(request)
            count          <- calls.get
            sent           <- remote.requests.get
            observed       <- events.get
          } yield assertTrue(
            first.errors.isEmpty,
            second.errors.isEmpty,
            rejected.errors.flatMap(codeOf) == List("REVOKED"),
            count == 3,
            sent.size == 2,
            observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Hit)) == (if (uncached) 0
                                                                                              else 1),
            observed.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Miss)) == (if (uncached) 0
                                                                                               else 1)
          )
        }
        .map(_.reduce(_ && _))
    },
    test("exposes only explicit resolver rejections, never defects or arbitrary Caliban errors") {
      val rejection = Rejection("Safe public message.", "PERSISTED_QUERY_NOT_FOUND")
      val secret    = "resolver-secret"
      val resolvers = List(
        PhaseHooks.resolution[Any](_ => ZIO.fail(rejection)),
        PhaseHooks.resolution[Any](_ => ZIO.die(rejection)),
        PhaseHooks.resolution[Any](_ => throw rejection),
        PhaseHooks.resolution[Any](_ => ZIO.fail(CalibanError.ExecutionError("private-message"))),
        PhaseHooks.resolution[Any](_ => ZIO.fail(rejection).ensuring(ZIO.dieMessage("private-finalizer"))),
        PhaseHooks.trustedDocuments(Map("value-v1" -> query))(_ => throw rejection),
        PhaseHooks.resolution[Any](_ => ZIO.fail(new RuntimeException(secret)), cacheable = false)
      )

      for {
        remote  <- stub(okResponse)
        results <- ZIO.foreach(resolvers) { resolver =>
                     remoteGateway(remote.endpoint)
                       .withPhaseHooks(resolver)
                       .interpreter
                       .flatMap(_.explain(request).either)
                   }
        errors   = results.flatMap(_.left.toOption)
        failure  = errors.lastOption.collect { case error: CalibanError.ExecutionError => error }
        cause    = failure.flatMap(_.innerThrowable)
      } yield assertTrue(
        errors.size == resolvers.size,
        errors.headOption.exists(_.msg == rejection.message),
        errors.headOption.flatMap(codeOf).contains(rejection.code),
        errors.headOption.exists(!OperationPreparation.isInternalFailure(_)),
        errors.drop(1).forall(_.msg == "Operation resolution failed."),
        errors.drop(1).forall(codeOf(_).isEmpty),
        errors.drop(1).forall(error => OperationPreparation.isInternalFailure(error)),
        failure.map(_.copy(msg = "Changed diagnostic text.")).exists(OperationPreparation.isInternalFailure),
        !OperationPreparation.isInternalFailure(
          CalibanError.ExecutionError(
            "Operation resolution failed.",
            innerThrowable = Some(new RuntimeException("other"))
          )
        ),
        cause.exists(_.getMessage == secret),
        cause.exists(_.getCause.getMessage == secret),
        !errors.exists(_.msg.contains(secret))
      )
    },
    test("preserves resolver interruption even when a finalizer dies with a rejection") {
      val resolver =
        PhaseHooks.resolution[Any](_ => ZIO.interrupt.ensuring(ZIO.die(Rejection("Not public.", "PRIVATE"))))
      for {
        remote  <- stub(okResponse)
        runtime <- remoteGateway(remote.endpoint).withPhaseHooks(resolver).interpreter
        exit    <- runtime.explain(request).exit
      } yield assertTrue(exit.causeOption.exists(_.isInterruptedOnly))
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
