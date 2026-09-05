package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.OperationResolver.Rejection
import caliban.gateway.internal.OperationHooks
import caliban.{ CalibanError, GraphQLRequest, InputValue, ResponseValue }
import caliban.Value.StringValue
import zio._
import zio.test._

object OperationResolverSpec extends ZIOSpecDefault {

  private val schema   = "type Query { value(input: String): String }"
  private val query    = "query Value($input: String) { value(input: $input) }"
  private val response = """{"data":{"value":"ok"}}"""
  private val request  = GraphQLRequest(
    operationName = Some("Value"),
    variables = Some(Map("input" -> StringValue("hello"))),
    extensions = Some(Map("documentId" -> StringValue("value-v1")))
  )

  private def documentId(request: GraphQLRequest): Option[String] =
    request.extensions.flatMap(_.get("documentId")).collect { case StringValue(id) => id }

  private def code(error: CalibanError): Option[ResponseValue] =
    field(error.toResponseValue, "extensions").flatMap(field(_, "code"))

  def spec = suite("OperationResolverSpec")(
    test("trusted documents override client text and preserve request fields, including on cache hits") {
      for {
        remote   <- stub(response)
        seen     <- Ref.make(List.empty[GraphQLRequest])
        runtime  <- Gateway
                      .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                      .withOperationResolver(OperationResolver.trustedDocuments(Map("value-v1" -> query))(documentId))
                      .withOperationPolicy(
                        OperationPolicy[Any](operation => seen.update(_ :+ operation.request).as(OperationPolicy.Allow))
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
        remote      <- stub(response)
        policyCalls <- Ref.make(0)
        runtime     <-
          Gateway
            .compose(Subgraph.graphql("remote", remote.endpoint, schema))
            .withOperationResolver(OperationResolver.trustedDocuments(Map("value-v1" -> query))(documentId))
            .withOperationPolicy(OperationPolicy[Any](_ => policyCalls.update(_ + 1).as(OperationPolicy.Allow)))
            .interpreter
        rejected    <- ZIO.foreach(invalid)(r => runtime.executeRequest(r.copy(query = Some(query))))
        missing     <- runtime.executeRequest(unknown.copy(query = Some(query)))
        sent        <- remote.requests.get
        calls       <- policyCalls.get
      } yield assertTrue(
        rejected.forall(_.errors.map(_.msg) == List("A non-empty trusted document ID is required.")),
        rejected.forall(_.errors.flatMap(code) == List(StringValue("TRUSTED_DOCUMENT_ID_INVALID"))),
        missing.errors.map(_.msg) == List("Trusted document not found."),
        missing.errors.flatMap(code) == List(StringValue("TRUSTED_DOCUMENT_NOT_FOUND")),
        sent.isEmpty,
        calls == 0
      )
    },
    test("uses the caller's extraction format and keeps IDs opaque") {
      val resolver = OperationResolver.trustedDocuments(Map(" opaque ID " -> query))(_.operationName)
      for {
        resolved <- resolver.resolve(GraphQLRequest(operationName = Some(" opaque ID ")))
        rejected <- resolver.resolve(GraphQLRequest(operationName = Some("opaque ID"))).either
      } yield assertTrue(
        resolved == query,
        rejected == Left(Rejection("Trusted document not found.", "TRUSTED_DOCUMENT_NOT_FOUND"))
      )
    },
    test("resolves explain requests while check still validates literal text") {
      for {
        remote   <- stub(response)
        runtime  <- Gateway
                      .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                      .withOperationResolver(OperationResolver.trustedDocuments(Map("value-v1" -> query))(documentId))
                      .interpreter
        plan     <- runtime.explain(request)
        rejected <- runtime.explain(request.copy(extensions = None)).either
        checked  <- runtime.check(query).exit
        invalid  <- runtime.check("{ missing }").exit
        sent     <- remote.requests.get
      } yield assertTrue(
        plan.contains("remote"),
        rejected.left.toOption.flatMap(code).contains(StringValue("TRUSTED_DOCUMENT_ID_INVALID")),
        checked.isSuccess,
        invalid.isFailure,
        sent.isEmpty
      )
    },
    test("resolves every request before cache lookup and only uncached bypasses preparation reuse") {
      ZIO
        .foreach(List(false, true)) { uncached =>
          for {
            remote           <- stub(response)
            recorded         <- recordEvents
            (events, wrapper) = recorded
            calls            <- Ref.make(0)
            resolve           = (_: GraphQLRequest) =>
                                  calls.updateAndGet(_ + 1).flatMap {
                                    case 1 | 2 => ZIO.succeed(query)
                                    case _     => ZIO.fail(Rejection("Revoked.", "REVOKED"))
                                  }
            resolver          = if (uncached) OperationResolver.uncached(resolve) else OperationResolver(resolve)
            runtime          <- (Gateway
                                  .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                                  .withOperationResolver(resolver) @@ wrapper).interpreter
            first            <- runtime.executeRequest(request)
            second           <- runtime.executeRequest(request.copy(extensions = Some(Map("documentId" -> StringValue("alias")))))
            rejected         <- runtime.executeRequest(request)
            count            <- calls.get
            sent             <- remote.requests.get
            observed         <- events.get
          } yield assertTrue(
            first.errors.isEmpty,
            second.errors.isEmpty,
            rejected.errors.flatMap(code) == List(StringValue("REVOKED")),
            count == 3,
            sent.size == 2,
            observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == (if (uncached) 0
                                                                                                      else 1),
            observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss)) == (if (uncached) 0
                                                                                                       else 1)
          )
        }
        .map(_.reduce(_ && _))
    },
    test("exposes only explicit resolver rejections, never defects or arbitrary Caliban errors") {
      val rejection = Rejection("Safe public message.", "PERSISTED_QUERY_NOT_FOUND")
      val resolvers = List(
        OperationResolver[Any](_ => ZIO.fail(rejection)),
        OperationResolver[Any](_ => ZIO.die(rejection)),
        OperationResolver[Any](_ => throw rejection),
        OperationResolver[Any](_ => ZIO.fail(CalibanError.ExecutionError("private-message"))),
        OperationResolver[Any](_ => ZIO.fail(rejection).ensuring(ZIO.dieMessage("private-finalizer"))),
        OperationResolver.trustedDocuments(Map("value-v1" -> query))(_ => throw rejection)
      )

      for {
        results <- ZIO.foreach(resolvers) { resolver =>
                     new OperationHooks[Any](_ => Nil, Some(resolver), None, GatewayWrapper.empty)
                       .resolve(request)
                       .either
                   }
        errors   = results.flatMap(_.left.toOption)
      } yield assertTrue(
        errors.size == resolvers.size,
        errors.headOption.exists(_.msg == rejection.message),
        errors.headOption.flatMap(code).contains(StringValue(rejection.code)),
        errors.headOption.exists(!OperationHooks.isInternalFailure(_)),
        errors.drop(1).forall(_.msg == "Operation resolution failed."),
        errors.drop(1).forall(code(_).isEmpty),
        errors.drop(1).forall(error => OperationHooks.isInternalFailure(error))
      )
    },
    test("preserves resolver interruption even when a finalizer dies with a rejection") {
      val resolver = OperationResolver[Any](_ => ZIO.interrupt.ensuring(ZIO.die(Rejection("Not public.", "PRIVATE"))))
      new OperationHooks[Any](_ => Nil, Some(resolver), None, GatewayWrapper.empty).resolve(request).exit.map { exit =>
        assertTrue(exit.causeOption.exists(_.isInterruptedOnly))
      }
    },
    test("does not expose resolver rejections returned by a policy") {
      for {
        remote  <- stub(response)
        runtime <- Gateway
                     .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                     .withOperationPolicy(OperationPolicy[Any](_ => ZIO.fail(Rejection("Private.", "PRIVATE"))))
                     .interpreter
        result  <- runtime.executeRequest(request.copy(query = Some(query)))
        sent    <- remote.requests.get
      } yield assertTrue(
        result.errors.map(_.msg) == List("Operation policy failed."),
        result.errors.forall(error => OperationHooks.isInternalFailure(error)),
        result.errors.forall(code(_).isEmpty),
        sent.isEmpty
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
