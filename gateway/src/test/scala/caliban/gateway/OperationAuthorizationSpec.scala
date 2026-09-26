package caliban.gateway

import caliban.Value.StringValue
import caliban.InputValue.ObjectValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.PhaseHooks.{ Denial, Rejection }
import caliban.gateway.internal.OperationPreparation
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio._
import zio.test._

object OperationAuthorizationSpec extends ZIOSpecDefault {

  private trait Documents {
    def resolve(id: String): UIO[String]
  }

  private trait Decisions {
    def allow(operationName: Option[String]): UIO[Boolean]
  }

  private val query   = "query Value($input: String) { value(input: $input) }"
  private val request = GraphQLRequest(
    query = Some(query),
    operationName = Some("Value"),
    variables = Some(Map("input" -> StringValue("hello")))
  )

  private def executionCause(response: GraphQLResponse[CalibanError]): Option[Throwable] =
    response.errors.collectFirst { case error: CalibanError.ExecutionError => error.innerThrowable }.flatten

  def spec = suite("OperationAuthorizationSpec")(
    test("resolves an identifier and evaluates policy with both environments and FiberRef context") {
      for {
        remote    <- stub(okResponse)
        observed  <- Ref.make(List.empty[String])
        context   <- FiberRef.make("missing")
        resolver   = PhaseHooks.resolution[Documents] { request =>
                       request.extensions.flatMap(_.get("operationId")) match {
                         case Some(StringValue(id)) => ZIO.serviceWithZIO[Documents](_.resolve(id))
                         case _                     => ZIO.fail(new IllegalArgumentException("operationId is required"))
                       }
                     }
        policy     = PhaseHooks.authorization[Decisions] { operation =>
                       for {
                         value   <- context.get
                         _       <- observed.update(value :: _)
                         allowed <- ZIO.serviceWithZIO[Decisions](_.allow(operation.executionRequest.operationName))
                         _       <- ZIO.fail(Denial()).unless(allowed)
                       } yield ()
                     }
        gateway    = (remoteGateway(remote.endpoint)
                       .withPhaseHooks(resolver)
                       .withPhaseHooks(policy): Gateway[Documents with Decisions])
        runtime   <- gateway.interpreter
        documents  = new Documents {
                       def resolve(id: String): UIO[String] =
                         if (id == "value-operation") ZIO.succeed(query)
                         else ZIO.dieMessage("unexpected operation identifier")
                     }
        decisions  = new Decisions {
                       def allow(operationName: Option[String]): UIO[Boolean] =
                         ZIO.succeed(operationName.contains("Value"))
                     }
        identified = request.copy(
                       query = None,
                       extensions = Some(Map("operationId" -> StringValue("value-operation")))
                     )
        result    <- context
                       .locally("request-context")(
                         runtime.executeRequest(identified)
                       )
                       .provideLayer(ZLayer.succeed(documents) ++ ZLayer.succeed(decisions))
        seen      <- observed.get
        sent      <- remote.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        seen == List("request-context"),
        sent.map(_.query) == Vector(Some(query)),
        sent.map(_.operationName) == Vector(Some("Value")),
        sent.map(_.variables) == Vector(Some(Map("input" -> StringValue("hello"))))
      )
    },
    test("runs policy only after validation and rejects without contacting a source") {
      for {
        remote    <- stub(okResponse)
        calls     <- Ref.make(0)
        policy     = PhaseHooks.authorization[Any] { _ =>
                       calls.update(_ + 1) *> ZIO.fail(Denial())
                     }
        runtime   <- remoteGateway(remote.endpoint)
                       .withPhaseHooks(policy)
                       .interpreter
        invalid   <- runtime.executeRequest(GraphQLRequest(query = Some("{ missing }")))
        malformed <- runtime.executeRequest(
                       request.copy(variables = Some(Map("input" -> ObjectValue(Map.empty))))
                     )
        rejected  <- runtime.executeRequest(request)
        runs      <- calls.get
        sent      <- remote.requests.get
      } yield assertTrue(
        invalid.errors.nonEmpty,
        malformed.errors.nonEmpty,
        rejected.errors.map(_.msg) == List("Operation denied."),
        runs == 1,
        sent.isEmpty
      )
    },
    test("composed authorization runs on cache hits and stops at the first denial") {
      for {
        remote         <- stub(okResponse)
        recorded       <- recordEvents
        (events, hooks) = recorded
        calls          <- Ref.make(List.empty[String])
        allowed        <- Ref.make(true)
        first           = PhaseHooks.authorization[Any](_ => calls.update(_ :+ "first"))
        second          = PhaseHooks.authorization[Any] { _ =>
                            calls.update(_ :+ "second") *> allowed.get.flatMap {
                              case true  => ZIO.unit
                              case false => ZIO.fail(Denial())
                            }
                          }
        last            = PhaseHooks.authorization[Any](_ => calls.update(_ :+ "last"))
        runtime        <- remoteGateway(remote.endpoint).withPhaseHooks(first ++ second ++ last ++ hooks).interpreter
        initial        <- runtime.executeRequest(request)
        cached         <- runtime.executeRequest(request)
        _              <- allowed.set(false)
        denied         <- runtime.executeRequest(request)
        observed       <- calls.get
        cachedEvents   <- events.get
        sent           <- remote.requests.get
      } yield assertTrue(
        initial.errors.isEmpty,
        cached.errors.isEmpty,
        denied.errors.map(_.msg) == List("Operation denied."),
        observed == List("first", "second", "last", "first", "second", "last", "first", "second"),
        cachedEvents.count(_ == PhaseHooks.Event.CacheAccess(PhaseHooks.CacheResult.Hit)) == 2,
        sent.size == 2
      )
    },
    test("exposes only explicit authorization denials without defects") {
      val denial = Denial("Safe reason.")
      val hooks  = List(
        PhaseHooks.authorization[Any](_ => ZIO.fail(denial)),
        PhaseHooks.authorization[Any](_ => ZIO.die(denial)),
        PhaseHooks.authorization[Any](_ => throw denial),
        PhaseHooks.authorization[Any](_ => ZIO.fail(denial).ensuring(ZIO.dieMessage("private-finalizer"))),
        PhaseHooks.authorization[Any](_ => ZIO.fail(CalibanError.ValidationError("private-reason", ""))),
        PhaseHooks.authorization[Any](_ => ZIO.fail(Rejection("Private.", "PRIVATE")))
      )
      for {
        remote  <- stub(okResponse)
        results <- ZIO.foreach(hooks) { hook =>
                     remoteGateway(remote.endpoint).withPhaseHooks(hook).interpreter.flatMap(_.executeRequest(request))
                   }
        sent    <- remote.requests.get
      } yield assertTrue(
        results.head.errors.map(_.msg) == List("Safe reason."),
        results.head.errors.forall(_.isInstanceOf[CalibanError.ValidationError]),
        results.tail.forall(_.errors.map(_.msg) == List("Operation authorization failed.")),
        results.tail.forall(_.errors.forall(error => OperationPreparation.isInternalFailure(error))),
        results.tail.forall(_.errors.forall(codeOf(_).isEmpty)),
        sent.isEmpty
      )
    },
    test("masks policy defects") {
      val secretPolicy = "policy-secret"

      for {
        remote        <- stub(okResponse)
        policyRuntime <-
          remoteGateway(remote.endpoint)
            .withPhaseHooks(
              PhaseHooks.authorization[Any](_ => ZIO.dieMessage(secretPolicy))
            )
            .interpreter
        policyResult  <- policyRuntime.executeRequest(request)
        sent          <- remote.requests.get
        policyCause    = executionCause(policyResult)
      } yield assertTrue(
        policyResult.errors.map(_.msg) == List("Operation authorization failed."),
        policyCause.exists(_.getMessage == secretPolicy),
        policyCause.exists(_.getCause.getMessage == secretPolicy),
        !policyResult.errors.exists(_.msg.contains(secretPolicy)),
        sent.isEmpty
      )
    },
    test("preserves hook interruption") {
      for {
        remote  <- stub(okResponse)
        started <- Promise.make[Nothing, Unit]
        policy   = PhaseHooks.authorization[Any](_ =>
                     started.succeed(()).unit *> ZIO.never.ensuring(ZIO.dieMessage("hook-finalizer-secret"))
                   )
        runtime <- remoteGateway(remote.endpoint)
                     .withPhaseHooks(policy)
                     .interpreter
        fiber   <- runtime.executeRequest(request).fork
        _       <- started.await
        exit    <- fiber.interrupt
        sent    <- remote.requests.get
      } yield assertTrue(exit.causeOption.exists(_.isInterruptedOnly), sent.isEmpty)
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
