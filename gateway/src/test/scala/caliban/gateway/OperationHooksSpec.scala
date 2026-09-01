package caliban.gateway

import caliban.Value.StringValue
import caliban.InputValue.ObjectValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationPolicy.{ Allow, Reject }
import caliban.gateway.internal.OperationHooks
import caliban.{ CalibanError, GraphQLRequest }
import zio._
import zio.test._

object OperationHooksSpec extends ZIOSpecDefault {

  private trait Documents {
    def resolve(id: String): UIO[String]
  }

  private trait Decisions {
    def allow(operationName: Option[String]): UIO[Boolean]
  }

  private val schema   = "type Query { value(input: String): String }"
  private val query    = "query Value($input: String) { value(input: $input) }"
  private val request  = GraphQLRequest(
    query = Some(query),
    operationName = Some("Value"),
    variables = Some(Map("input" -> StringValue("hello")))
  )
  private val response = """{"data":{"value":"ok"}}"""

  def spec = suite("OperationHooksSpec")(
    test("uses request text directly when no resolver is configured") {
      for {
        remote  <- stub(response)
        runtime <- Gateway.compose(Subgraph.graphql("remote", remote.endpoint, schema)).interpreter
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
        remote    <- stub(response)
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
        gateway    = (Gateway
                       .compose(Subgraph.graphql("remote", remote.endpoint, schema))
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
        remote    <- stub(response)
        calls     <- Ref.make(0)
        policy     = OperationPolicy[Any] { _ =>
                       calls.update(_ + 1).as(Reject())
                     }
        runtime   <- Gateway
                       .compose(Subgraph.graphql("remote", remote.endpoint, schema))
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
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.graphql("remote", remote.endpoint, schema))
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
        remote          <- stub(response)
        resolverRuntime <- Gateway
                             .compose(Subgraph.graphql("resolver", remote.endpoint, schema))
                             .withOperationResolver(
                               OperationResolver.uncached[Any](_ => ZIO.fail(new RuntimeException(secretResolver)))
                             )
                             .interpreter
        resolverResult  <- resolverRuntime.executeRequest(request)
        policyRuntime   <- Gateway
                             .compose(Subgraph.graphql("policy", remote.endpoint, schema))
                             .withOperationPolicy(
                               OperationPolicy[Any](_ => ZIO.dieMessage(secretPolicy))
                             )
                             .interpreter
        policyResult    <- policyRuntime.executeRequest(request)
        sent            <- remote.requests.get
        messages         = (resolverResult.errors ::: policyResult.errors).map(_.msg)
        resolverCause    = resolverResult.errors.collectFirst { case error: CalibanError.ExecutionError =>
                             error.innerThrowable
                           }.flatten
        policyCause      = policyResult.errors.collectFirst { case error: CalibanError.ExecutionError =>
                             error.innerThrowable
                           }.flatten
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
        remote  <- stub(response)
        started <- Promise.make[Nothing, Unit]
        policy   = OperationPolicy[Any](_ =>
                     started.succeed(()).unit *> ZIO.never.ensuring(ZIO.dieMessage("hook-finalizer-secret"))
                   )
        runtime <- Gateway
                     .compose(Subgraph.graphql("remote", remote.endpoint, schema))
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
        GatewayWrapper.empty
      )
      val bypass = new OperationHooks[Any](
        _ => Nil,
        Some(OperationResolver.uncached[Any](_ => ZIO.succeed(query))),
        None,
        GatewayWrapper.empty
      )

      assertTrue(
        stable.cacheable,
        !bypass.cacheable
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
