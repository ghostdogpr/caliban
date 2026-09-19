package caliban.gateway

import caliban.Value.StringValue
import caliban.InputValue.ObjectValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.{ Allow, Reject }
import caliban.gateway.internal.OperationHooks
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio._
import zio.http.URL
import zio.test._

object OperationHooksSpec extends ZIOSpecDefault {

  private trait Documents {
    def resolve(id: String): UIO[String]
  }

  private trait Decisions {
    def allow(operationName: Option[String]): UIO[Boolean]
  }

  private val schema  = "type Query { value(input: String): String }"
  private val query   = "query Value($input: String) { value(input: $input) }"
  private val request = GraphQLRequest(
    query = Some(query),
    operationName = Some("Value"),
    variables = Some(Map("input" -> StringValue("hello")))
  )

  private def remoteGateway(endpoint: URL, name: String = "remote"): Gateway[Any] =
    Gateway.compose(Subgraph.graphql(name, endpoint, schema))

  private def executionCause(response: GraphQLResponse[CalibanError]): Option[Throwable] =
    response.errors.collectFirst { case error: CalibanError.ExecutionError => error.innerThrowable }.flatten

  def spec = suite("OperationHooksSpec")(
    test("uses request text directly when no resolver is configured") {
      for {
        remote  <- stub(okResponse)
        runtime <- remoteGateway(remote.endpoint).interpreter
        result  <- runtime.executeRequest(request)
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        sent.map(_.query) == Vector(Some(query)),
        sent.map(_.operationName) == Vector(Some("Value")),
        sent.map(_.variables) == Vector(Some(Map("input" -> StringValue("hello"))))
      )
    },
    test("resolves an identifier and evaluates policy with both environments and FiberRef context") {
      for {
        remote    <- stub(okResponse)
        observed  <- Ref.make(List.empty[String])
        context   <- FiberRef.make("missing")
        resolver   = OperationResolver[Documents] { request =>
                       request.extensions.flatMap(_.get("operationId")) match {
                         case Some(StringValue(id)) => ZIO.serviceWithZIO[Documents](_.resolve(id))
                         case _                     => ZIO.fail(new IllegalArgumentException("operationId is required"))
                       }
                     }
        policy     = OperationPolicy[Decisions] { operation =>
                       for {
                         value   <- context.get
                         _       <- observed.update(value :: _)
                         allowed <- ZIO.serviceWithZIO[Decisions](_.allow(operation.executionRequest.operationName))
                       } yield if (allowed) Allow else Reject()
                     }
        gateway    = (remoteGateway(remote.endpoint)
                       .withOperationResolver(resolver)
                       .withOperationPolicy(policy): Gateway[Documents with Decisions])
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
        policy     = OperationPolicy[Any] { _ =>
                       calls.update(_ + 1).as(Reject())
                     }
        runtime   <- remoteGateway(remote.endpoint)
                       .withOperationPolicy(policy)
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
        rejected.errors.map(_.msg) == List("Operation rejected by gateway policy."),
        runs == 1,
        sent.isEmpty
      )
    },
    test("returns an explicit public policy rejection reason") {
      for {
        remote  <- stub(okResponse)
        runtime <- remoteGateway(remote.endpoint)
                     .withOperationPolicy(OperationPolicy[Any](_ => ZIO.succeed(Reject("Operation denied."))))
                     .interpreter
        result  <- runtime.executeRequest(request)
        sent    <- remote.requests.get
      } yield assertTrue(result.errors.map(_.msg) == List("Operation denied."), sent.isEmpty)
    },
    test("masks resolver failures and policy defects") {
      val secretResolver = "resolver-secret"
      val secretPolicy   = "policy-secret"

      for {
        remote          <- stub(okResponse)
        resolverRuntime <- remoteGateway(remote.endpoint, "resolver")
                             .withOperationResolver(
                               OperationResolver.uncached[Any](_ => ZIO.fail(new RuntimeException(secretResolver)))
                             )
                             .interpreter
        resolverResult  <- resolverRuntime.executeRequest(request)
        policyRuntime   <- remoteGateway(remote.endpoint, "policy")
                             .withOperationPolicy(
                               OperationPolicy[Any](_ => ZIO.dieMessage(secretPolicy))
                             )
                             .interpreter
        policyResult    <- policyRuntime.executeRequest(request)
        sent            <- remote.requests.get
        messages         = (resolverResult.errors ::: policyResult.errors).map(_.msg)
        resolverCause    = executionCause(resolverResult)
        policyCause      = executionCause(policyResult)
      } yield assertTrue(
        resolverResult.errors.map(_.msg) == List("Operation resolution failed."),
        policyResult.errors.map(_.msg) == List("Operation policy failed."),
        resolverCause.exists(_.getMessage == secretResolver),
        policyCause.exists(_.getMessage == secretPolicy),
        !messages.exists(_.contains(secretResolver)),
        !messages.exists(_.contains(secretPolicy)),
        sent.isEmpty
      )
    },
    test("preserves hook interruption") {
      for {
        remote  <- stub(okResponse)
        started <- Promise.make[Nothing, Unit]
        policy   = OperationPolicy[Any](_ =>
                     started.succeed(()).unit *> ZIO.never.ensuring(ZIO.dieMessage("hook-finalizer-secret"))
                   )
        runtime <- remoteGateway(remote.endpoint)
                     .withOperationPolicy(policy)
                     .interpreter
        fiber   <- runtime.executeRequest(request).fork
        _       <- started.await
        exit    <- fiber.interrupt
        sent    <- remote.requests.get
      } yield assertTrue(exit.causeOption.exists(_.isInterruptedOnly), sent.isEmpty)
    },
    test("only uncached resolvers bypass the operation cache") {
      val stable = new OperationHooks[Any](
        _ => Nil,
        Some(OperationResolver[Any](_ => ZIO.succeed(query))),
        Some(OperationPolicy[Any](_ => ZIO.succeed(Allow))),
        PhaseHooks.empty
      )
      val bypass = new OperationHooks[Any](
        _ => Nil,
        Some(OperationResolver.uncached[Any](_ => ZIO.succeed(query))),
        None,
        PhaseHooks.empty
      )

      assertTrue(
        stable.cacheable,
        !bypass.cacheable
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
