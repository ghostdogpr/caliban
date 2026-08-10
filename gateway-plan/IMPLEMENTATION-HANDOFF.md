# Caliban Gateway Implementation Handoff

Status: implementation-ready

This is the canonical entry point for implementing the embedded Caliban Gateway. An implementation agent should read this document completely before editing code. The plan overview, decisions, and tickets preserve rationale and deeper detail, but the implementation must not depend on reconstructing the architecture from them.

## Instructions for the implementation agent

1. Start from a fresh branch based on the then-current `series/3.x`. Do not base implementation on `wip_gateway`.
2. Treat the gateway code currently present on `wip_gateway` as an abandoned prototype. Its `SuperGraph`, `SubGraph`, ZQuery routing, `Extend`, and `ResponseValue` execution APIs impose no compatibility constraint.
3. Read [the gateway domain language](CONTEXT.md) after this document and use its terms consistently.
4. Implement the ticket milestones below in dependency order. Each milestone must end in executable behavior and acceptance checks; do not build all internals independently and connect them at the end.
5. Do not reopen settled architecture because another design looks locally simpler. Reopen a decision only when compilation, a specification conflict, the selected acceptance suites, or measurements demonstrate that the stated contract cannot work.
6. Private representations and algorithms may change freely behind the fixed seams. When a performance seam is called out, measure before specializing it.
7. The milestone sequence and ticket dependency graph are one plan. If they ever disagree, reconcile the documents before writing code instead of explaining the mismatch separately.

## Goal

Add a production-capable, ZIO-native, embedded GraphQL gateway based on Caliban. It must code-first compose and execute:

- ordinary remote GraphQL APIs;
- Federation-enabled remote GraphQL subgraphs;
- in-process Caliban graphs;
- graphs mixing any of those source kinds.

The gateway should feel like a Caliban library, exploit Scala and ZIO directly, reuse Caliban semantics where that remains competitive, and retain private specialization seams where generic Caliban representations would prevent performance comparable to leading gateways.

Start `caliban-gateway` cross-built on the repository's then-current supported JVM Scala matrix. Keep the common implementation Scala 2-compatible when that costs little. This is not a lowest-common-denominator constraint: if a substantive Scala 3 feature materially improves the public model, correctness, maintainability, or measured hot path and cannot reasonably be isolated, the module may become Scala 3-only with the concrete reason recorded before its API is published. Dependency availability is also a valid reason to narrow the matrix. Ticket 1 provisionally settles the matrix by compiling the skeleton and Ticket 34 re-confirms it against the real planner/executor. Do not publish snapshots, milestones, release candidates, or releases of `caliban-gateway` before that confirmation, because narrowing an already published matrix is a user-visible break.

The initial public performance objective is semantically correct throughput within 15% of the leader in the current GraphQL Gateways Benchmark. The initial compatibility objective is every in-scope case in the current Federation Gateway Audit through the gateway's native code-first composition path.

## Evidence and version policy

The competitor source checkouts and research under `gateway-plan` are dated research snapshots. They preserve the evidence behind the architecture, but they are not versions the implementation must target.

When an implementation slice first integrates the Federation Gateway Audit or GraphQL Gateways Benchmark, select the latest available upstream revision and commit the exact revisions to `gateway-compatibility/versions.conf`. Workflows read only that file, so unrelated pull requests never float to new upstream code. Refreshes are explicit reviewed commits. Before release, refresh both suites and Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion to their latest suitable stable releases. Resolve those moving inputs plus image digests and effective configurations in each run manifest so an individual result remains reproducible. Do not use floating versions inside a run, but do not freeze the project to the planning-session snapshots either.

When a specification or competitor behavior must be revisited, consult the current specification and current competitor versions first. Historical source snapshots remain useful for rationale and change comparison.

Decision hierarchy:

1. GraphQL September 2025, GraphQL-over-HTTP, and applicable Federation specifications.
2. Explicit Caliban Gateway decisions where specifications permit alternatives.
3. Checked-in semantic tests and expected source-call invariants.
4. Majority behavior across the four competitors where genuine latitude remains.

Vendor-specific error wording, proprietary codes, serialized plans, and internal plan shapes are not normative.

## Scope

### Included

- One published JVM `caliban-gateway` module, cross-built where the implementation and required dependencies permit.
- A pure, code-first composition description and scoped build.
- Queries and mutations over unary GraphQL-over-HTTP JSON source calls.
- GraphQL September 2025 operation semantics.
- Core Federation 2 composition/execution through entity interfaces, including the in-scope audit features.
- Ordinary and local explicit keys/lookups, native batch lookups, requirements, and shareability.
- Caliban-compatible structured execution plus a direct encoded response path.
- Scoped lifecycle, cancellation, finite deadlines and limits, bounded caches/admission, conservative query retries, metrics, and final-phase tracing.
- A Quick Adapter path that preserves direct encoding for real users and benchmarks.

### Explicitly deferred

- Serialized composition/execution artifacts, JOIN artifact loading, offline composition, hot reload, and cross-generation retirement.
- Standalone router, CLI, native-image packaging, process configuration, probes, signals, and deployment packaging.
- Subscriptions, `@defer`, `@stream`, multipart upload, and other incremental/streaming execution.
- gRPC, REST, or other non-GraphQL execution protocols.
- GraphQL Composite Schemas conformance. The model is Composite-inspired, not a conformance claim.
- Built-in authorization, rate limiting, traffic shaping, response caching, circuit breaking, hedging, fallback routing, and plugin marketplaces.
- Compatibility with the abandoned prototype API.

## Product and module shape

### Published modules

`caliban-gateway` is the only new initial gateway artifact. It contains public code-first descriptions and the runtime plus all private composition, frontend, planner, execution, source, and response machinery. It starts on the repository's normal JVM cross-build matrix and narrows only for a recorded language or dependency reason.

Create another artifact only when an integration introduces a meaningful extra dependency/runtime target or a genuinely independent compatibility surface. OpenTelemetry qualifies when it is implemented at the end: add an optional `caliban-gateway-tracing` module only then, rather than making the existing cross-built tracing module depend on gateway or adding OpenTelemetry to gateway core. If the gateway remains cross-built, the tracing module follows every Scala version supported by both gateway and its tracing dependencies; it may use a narrower matrix for a concrete dependency reason.

Do not create `caliban-gateway-router`, `caliban-gateway-quick`, a planner artifact, a connector artifact, or placeholder tracing modules.

### Changes to existing modules

- Caliban core receives a generally useful bounded parser overload; an optional unary encoded-interpreter capability and generic response classification; a generic structured server-failure error carrying the same classification; and core-owned `IncomingHeaders` plus its narrowly scoped FiberRef context. Existing parser and `GraphQLInterpreter` method signatures remain source-compatible. Adding `CalibanError.ResponseError` extends Caliban's public sealed error hierarchy: it is binary-additive, but source consumers that exhaustively match every current subtype may receive a new exhaustivity warning, fail under fatal warnings, or need a fallback case. Treat that as an explicit reviewed core compatibility change and cover Caliban-owned matches in the cross-built test matrix.
- Quick Adapter recognizes that capability and returns its bytes directly for unary HTTP requests. Existing structured interpreter execution remains unchanged, but Quick itself gains two reviewed adapter behavior changes on both structured and encoded paths: a configurable always-finite unary body limit, and GraphQL-over-HTTP-compliant `405 Method Not Allowed` for mutation-over-GET. Call both out in release notes.
- Other HTTP adapters may adopt the same core capability later; only Quick changes initially.

### Existing-module and repository integration

- `caliban-gateway` may compile-depend on `caliban-tools` and use its introspection client with the gateway-owned sttp `Backend[Task]`; the resulting client/sttp dependencies are intentional because remote execution already needs them.
- Do not depend on `caliban-stitching` or deprecate it in this milestone. The products coexist until the gateway is proven and a separate migration decision exists.
- `caliban-federation` is a test/example dependency for constructing representative subgraphs. Production composition normalizes Federation SDL through core parser/introspection ASTs and does not depend on federation server support.
- Add non-published audit and benchmark projects only at the milestones stated below, following the existing `apollo-compatibility` assembly/container/workflow precedent.
- A new published artifact begins with `mimaPreviousArtifacts := Set.empty`; establish its real MiMa baseline only after the first release. Examples must compile in a Scala-matrix test/example project. The current documentation/mdoc project is not evidence for Scala 3 examples until its own matrix supports them.

### Package boundaries

Public API belongs under `caliban.gateway`. Internal packages should follow the concepts below—composition, frontend, planning, execution, response, and sources—but remain `private[gateway]`. Exact private package/file layout is deliberately not a compatibility decision.

Dependencies point from gateway to Caliban modules, never from Caliban core to gateway. The core encoded capability must be generic; Quick must not depend on gateway, and gateway must not depend on zio-http.

## Public API contract

The following sketch fixes shape and semantics, not every final identifier or private representation. It uses the same ordinary-method shape on every supported Scala version; the repository's existing `R & R1` environment syntax already cross-compiles.

```scala
package caliban.gateway

import caliban._
import caliban.parsing.adt.Document
import sttp.model.Uri
import zio._

final class Subgraph[-R] private[gateway] (/* private immutable description */) {
  def lookup(
    entity: String,
    key: String,
    field: String,
    arguments: Map[String, ArgumentMapping]
  ): Subgraph[R] = ???

  def batchLookup(
    entity: String,
    key: String,
    field: String,
    arguments: Map[String, ArgumentMapping],
    correlation: BatchCorrelation
  ): Subgraph[R] = ???

  def shareable(entity: String, fields: String): Subgraph[R] = ???
  def requireArguments(
    field: String,
    fromParent: Map[String, ArgumentMapping]
  ): Subgraph[R] = ???
  def headers[R1](policy: HeaderPolicy[R1]): Subgraph[R & R1] = ???
  def transform(transformer: GatewayTransformer): Subgraph[R] = ???
}

object Subgraph {
  def graphql(name: String, endpoint: Uri): Subgraph[Any] = ???
  def graphql[R](name: String, endpoint: Uri, config: RemoteSubgraphConfig[R]): Subgraph[R] = ???

  def federated(name: String, endpoint: Uri): Subgraph[Any] = ???
  def federated[R](name: String, endpoint: Uri, config: RemoteSubgraphConfig[R]): Subgraph[R] = ???

  def caliban[R](
    name: String,
    api: GraphQL[R],
    config: LocalSubgraphConfig = LocalSubgraphConfig.default
  ): Subgraph[R] = ???
}

sealed trait ArgumentMapping
object ArgumentMapping {
  final case class Parent(path: String) extends ArgumentMapping
  final case class InputObject(fields: Map[String, ArgumentMapping]) extends ArgumentMapping

  def parent(path: String): ArgumentMapping = ???
  def obj(fields: (String, ArgumentMapping)*): ArgumentMapping = ???
  def paths(entries: (String, String)*): Map[String, ArgumentMapping] = ???
}

sealed trait BatchCorrelation
object BatchCorrelation {
  case object Ordered extends BatchCorrelation
  final case class ByKey(selection: String) extends BatchCorrelation
}

sealed trait SchemaSource
object SchemaSource {
  final case class Introspection(endpoint: Option[Uri] = None) extends SchemaSource
  final case class FederationService(endpoint: Option[Uri] = None) extends SchemaSource
  final case class Parsed(document: Document) extends SchemaSource
  final case class SDL(value: String) extends SchemaSource
}

final class Gateway[-R] private[gateway] (/* private immutable composition description */) {
  def withConfig(config: GatewayConfig): Gateway[R] = ???
  def resolveOperationsWith[R1](resolver: OperationResolver[R1]): Gateway[R & R1] = ???
  def authorizeWith[R1](policy: OperationPolicy[R1]): Gateway[R & R1] = ???
  def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] = ???
}

object Gateway {
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] = ???
  def from[R](subgraphs: NonEmptyChunk[Subgraph[R]]): Gateway[R] = ???
}

trait GatewayRuntime[-R]
    extends EncodedGraphQLInterpreter[R, CalibanError] {

  def schema: Document
  def render: String
  def renderCompact: String
  def buildDiagnostics: Chunk[CompositionDiagnostic]

  def executeRequest(
    request: GraphQLRequest,
    incomingHeaders: IncomingHeaders,
    options: ExecutionOptions
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]]

  def executeEncoded(
    request: GraphQLRequest,
    format: GraphQLResponseFormat,
    incomingHeaders: IncomingHeaders,
    options: ExecutionOptions
  )(implicit trace: Trace): URIO[R, EncodedGraphQLResponse]

  def explain(
    query: String,
    operationName: Option[String] = None
  )(implicit trace: Trace): IO[CalibanError, PlanExplanation]

  def drain(implicit trace: Trace): UIO[Unit]
  def status(implicit trace: Trace): UIO[GatewayStatus]
}

final case class GatewayConfig(
  composition: CompositionConfig = CompositionConfig.default,
  operations: OperationConfig = OperationConfig.default,
  planning: PlanningConfig = PlanningConfig.default,
  transport: TransportConfig = TransportConfig.default,
  execution: GatewayExecutionConfig = GatewayExecutionConfig.default,
  response: ResponseConfig = ResponseConfig.default
)
```

The supporting public types follow these contracts even if compilation refines names:

- `RemoteSubgraphConfig[-R]` is a final immutable private-constructor value with fluent methods for `SchemaSource`, static schema-acquisition headers, finite schema-acquisition limits, runtime `HeaderPolicy[R]`, remote execution limits, retry policy, and source-error policy. Its default uses introspection for an ordinary source and `_service { sdl }` for a Federation source. A distinct acquisition endpoint lives only in `SchemaSource`; the execution endpoint remains the constructor argument.
- `LocalSubgraphConfig` contains local execution limits and masking/import behavior only. It has no HTTP headers, retry policy, or endpoint.
- Core `caliban.IncomingHeaders` is an immutable, case-insensitive multi-map with validated names and values. Core stores it in a narrowly scoped FiberRef context that Quick installs around execution; it is not a universal request context and does not expose zio-http types. An explicit runtime-overload argument wins over the FiberRef value; the FiberRef is fallback for inherited interpreter calls.
- `HeaderPolicy[-R]` is an immutable ordered program built from typed operations: select named incoming headers, explicitly forward all, add static values, add effectfully computed values from `R`, and remove names. Effectful failure becomes a typed source request failure before transport dispatch.
- `OperationResolver[-R]` resolves absent/identified operation text to canonical query text or a closed resolver failure. `OperationPolicy[-R]` returns allow or a closed rejection from the read-only operation view described below. Neither interface receives mutable engine objects.
- `CompositionDiagnostic` has severity, stable code, safe message, optional source name, and original source coordinates. `GatewayBuildError` contains a non-empty chunk of diagnostics; ordering is severity/code/source/coordinate/message and is independent of acquisition completion order.
- `ExecutionOptions` initially contains only optional stricter request deadline and stricter resource limits. It has no fields that can change validation, schema visibility, planning, routing, retry, masking, or policy.
- `GatewayExecutionConfig` contains the finite request deadline, admission/active bounds, and default source-call limits. The deadline initiates interruption; it is a strict response deadline for gateway-owned and cooperative work, but cannot force an arbitrary uninterruptible user-provided effect to terminate on the JVM. This includes local Caliban execution, operation resolvers, operation policies, and effectful header policies.
- `GatewayTransformer` is a closed immutable structural transform modeled after Caliban's transformer API. V1 constructors cover rename type/field/argument and exclude field/input-field/argument/directives. Composition applies the matching schema transform and the inverse downstream/local input translation together; arbitrary resolver-changing `Transformer` subclasses are not accepted.
- `ArgumentMapping` is a closed immutable tree containing parent leaf paths and input objects. `ArgumentMapping.paths("id" -> "id")` is the concise scalar form. A batch lookup evaluates the mapping once per deduplicated entity and automatically lifts the resulting scalar or input object values into the source list argument.
- Core `GraphQLResponseOutcome` is gateway-neutral GraphQL-over-HTTP semantic metadata usable by any encoded interpreter. Core `CalibanError.ResponseError` may carry only `RequestError(kind)` or `ServerFailure(kind)`, a safe message, and GraphQL extensions; it cannot claim `Executed`. Ordinary resolver/field `ExecutionError`s remain executed GraphQL errors.

A representative remote configuration is:

```scala
final class RemoteSubgraphConfig[-R] private[gateway] (
  val schema: SchemaSource,
  val schemaHeaders: StaticHeaders,
  val acquisition: SchemaAcquisitionConfig,
  val runtimeHeaders: HeaderPolicy[R],
  val execution: SourceExecutionConfig,
  val retry: RetryConfig,
  val errorPolicy: SourceErrorPolicy
)
```

Each field has immutable fluent replacement methods and typed defaults. `SchemaAcquisitionConfig` has finite total timeout, decoded-body bytes, SDL/introspection nodes/depth, and parser limits; redirects are disabled. `SourceExecutionConfig` contains optional per-source overrides only for request-level behavior: timeout, logical-call permits, redirects, and request/body/JSON-structure limits. Backend-level connection pools, proxy, TLS, protocol, and decompression configuration remain global `TransportConfig` in v1 so one owned pooled backend has one coherent configuration. A disable-able or inheritable facility uses an `Inherited | Disabled | Configured(value)` ADT rather than overloading `None`. Independent safety caps resolve to the strictest finite value. `SchemaSource` is therefore consumed directly by `RemoteSubgraphConfig`, not by a separate constructor family.

Schema acquisition is intentionally cold and does not require `R`: `build` remains `ZIO[Scope, GatewayBuildError, GatewayRuntime[R]]`. Acquisition may use only static configured headers. An application that needs rotating or effectfully obtained acquisition credentials resolves them before constructing the description, supplies pinned SDL/`Document`, or rebuilds with new static values. Runtime `HeaderPolicy[R]` is evaluated only for execution calls. This prevents a misleading `build` signature whose acquisition secretly needs request services.

`Subgraph`, `Gateway`, and `GatewayRuntime` preserve contravariant ZIO environment requirements and infer intersections across local sources, effectful runtime header policies, operation resolution, and operation policy. Descriptions are final immutable values with private constructors and ordinary fluent copy methods, not public case classes. Their private representation can evolve without exposing constructor or `copy` compatibility.

Source names are case-sensitive strings. Build requires them to be nonblank, control-character-free, and unique exactly as written. It neither trims nor imposes GraphQL-name syntax.

Public endpoints use `sttp.model.Uri` and must be absolute HTTP(S) URIs. Remote execution and schema acquisition endpoints may differ.

`Gateway.from` accepts a `NonEmptyChunk`, while `Gateway.compose(first, rest*)` is the varargs convenience. There is no shallow description-error type. Invalid names, URIs, lookup metadata, acquired schemas, and composition rules are accumulated as `CompositionDiagnostic`s by `build`; `GatewayBuildError` contains a non-empty, deterministically ordered collection of those diagnostics.

The sketch deliberately has no default arguments on the richer execution overloads. The inherited one-argument `executeRequest` and two-argument `executeEncoded(request, format)` use the core incoming-header FiberRef fallback and `ExecutionOptions.default`. Direct embedded callers that need either value call the explicit overloads; their explicit headers take precedence. Quick supplies the FiberRef value but uses default gateway execution options initially.

### Structured and encoded execution

The runtime implements normal `GraphQLInterpreter` behavior for compatibility. Only that boundary materializes `GraphQLResponse[CalibanError]` and a generic response-value tree.

The native encoded path projects directly from the internal response store into caller-owned bytes. `GraphQLResponseFormat` is a closed core ADT for `Json` and `GraphQLResponseJson`, each with its canonical UTF-8 media type. `EncodedGraphQLResponse` owns an immutable byte chunk, the selected format, a bounded optional cache-control directive, and the generic core `GraphQLResponseOutcome`:

- `RequestError(kind)` where kind is operation resolution, parsing, validation, variable coercion, mutation-over-GET, policy rejection, or structural limit;
- `Executed`, including partial data, source GraphQL errors, and integrated source failures;
- `ServerFailure(kind)` where kind is overload, draining, deadline, output limit, or internal failure.

The metadata is a closed typed model, not arbitrary headers, status integers, or extension maps. Bytes are always a complete GraphQL response envelope. Core adds a generic structured `CalibanError.ResponseError` carrying the same request/server classification so Quick can classify a materialized `GraphQLResponse` without depending on gateway. Gateway-authored stable codes remain ordinary GraphQL error extensions.

Quick maps both surfaces identically without inspecting encoded bytes:

| Outcome | `application/graphql-response+json` | `application/json` |
| --- | --- | --- |
| Mutation-over-GET | `405` | `405` |
| Parsing, validation, coercion, resolution, policy, or structural request failure | `400` | Preserve legacy Quick request-error behavior: `200` |
| Executed GraphQL result, including partial/source errors | `200` | `200` |
| Overload or draining | `503` | `503` |
| Request deadline | `504` | `504` |
| Output limit or internal server failure | `500` | `500` |

The encoded cache-control value is the same bounded directive Quick currently derives from structured response extensions; Quick installs it as a header and omits its internal extension from encoded bytes just as today. The generic encoded capability retains this field so ordinary interpreters can provide parity. Caliban Gateway v1 always emits `None`: it drops source top-level extensions and does not implement cross-subgraph `@cacheControl` or `Caching` aggregation in this milestone. Requests negotiating server-sent events, and every upload, subscription, or incremental result, retain the structured path.

The table deliberately changes current Quick behavior in three places. Mutation-over-GET becomes `405` on both paths to follow GraphQL-over-HTTP, and Quick includes `Allow: POST` on that response. Gateway coercion, resolution, policy, and structural request errors become `400` for `application/graphql-response+json`, even where an ordinary current Caliban interpreter would be encoded as `200`. Gateway overload, draining, deadline, output-limit, and internal failures use `503`/`504`/`500` even for legacy `application/json`, instead of being flattened to an ordinary `200` GraphQL response. These are typed gateway/adapter semantics, not an invitation to reclassify ordinary resolver errors.

The core `EncodedGraphQLInterpreter` capability extends `GraphQLInterpreter`, takes a `GraphQLRequest` and negotiated `GraphQLResponseFormat`, and returns encoded bytes plus that semantic metadata. It is the minimal two-argument capability and has no gateway-specific options or header types.

Quick Adapter detects and retains that capability when its handler is constructed; it must not rediscover it after `GraphQLInterpreter` combinators have erased the subtype. Quick's configured effect scope surrounds the whole encoded call just as it surrounds structured execution. Introspection enablement, mutation rejection on GET, skip-validation, and configured validations keep their existing meanings; only the GET-mutation HTTP status changes from `400` to `405`. `skipValidation` may bypass configured GraphQL validation rules, but it never bypasses operation selection, bounded parsing/coercion, topology/type/input compatibility required to compile a safe call, or the plan/DAG verifier. A non-default validation configuration or skip setting contributes a stable discriminator or bypasses the gateway planned-operation cache. Quick's `queryExecution` setting never replaces the gateway scheduler; it remains visible to an invoked local Caliban interpreter. Quick's `queryCache` remains relevant only to ordinary/local Caliban execution, not the gateway plan cache. Quick populates the core incoming-header context around either path without widening the interpreter environment.

Quick bounds the raw unary HTTP request body before materializing an array or `GraphQLRequest`; the default is finite and a host may only select another finite limit. Body overflow returns `413`, while malformed or empty request encoding retains Quick's `400` behavior. This is a reviewed behavior change for every Quick user because current Quick materializes `Body.asString`/`Body.asArray` without its own cap. Document the default and migration in release notes; deployments that intentionally accept larger requests must select a larger finite value rather than disable the limit or inherit a possibly unbounded ambient setting. After decoding, gateway checks supplied raw query bytes and the variable/extension node and depth limits before invoking `OperationResolver`. Coercion remains after operation selection. These adapter-level failures remain outside the GraphQL engine.

Quick's gateway usage passes the raw `GatewayRuntime[R]`/`EncodedGraphQLInterpreter[R, E]` to route construction and supplies `R` through the route environment. Calling a final `GraphQLInterpreter.provide*`, `mapError`, or `wrapExecutionWith` first intentionally yields a plain interpreter and loses the optional encoded capability; document this until core offers capability-preserving decorators.

`application/json` and `application/graphql-response+json` negotiation, GET mutation rejection, request-error status, and gateway-failure status must match the equivalent structured Quick behavior. Add parity tests before the benchmark uses this path.

The inherited `check(query)` preserves Caliban's parse-and-static-validate meaning against the composed client schema. It applies the gateway's bounded parser and configured static validations, but does not resolve persisted operations, select one operation by name, coerce variables, authorize, plan, admit execution, or call a source. It may reuse parsed-document and whole-document static-validation cache entries, but never creates a selected `PreparedOperation` or inserts an executable plan merely because `check` succeeded.

## Composition model

`Gateway.compose` creates a pure reusable composition description. Scoped `build` performs schema acquisition, validation, normalization, merging, satisfiability checking, topology compilation, source construction, and runtime acquisition.

A subgraph contains stable identity, schema acquisition, composition capabilities, and execution configuration. It is not an endpoint or transport pool.

Initial subgraph kinds:

- ordinary remote GraphQL, defaulting to introspection;
- Federation-enabled remote GraphQL, defaulting to `_service { sdl }`;
- in-process Caliban, reading its schema and interpreter from `GraphQL[R]`;
- pinned `Document` or SDL as an alternative schema source for either remote kind.

Normalize all sources into one internal schema/capability model. There is no graph-wide Federation-versus-stitching mode. Federation changes capabilities—keys, `_entities`, directives—not the GraphQL transport.

Composition rules:

- compatible unique root fields coexist automatically;
- same-named compatible types merge and differently owned compatible fields combine;
- incompatible definitions fail composition;
- multiple providers fail unless the field is explicitly shareable;
- key fields may repeat as identity;
- matching names never infer a join;
- no automatic namespacing;
- subgraph-local transformations happen before final composition and preserve source-coordinate mapping.

Initial transformation support follows Caliban's existing transformer approach but is restricted to the closed `GatewayTransformer` structural operations that can be applied symmetrically to validation and downstream/local calls. Transformations belong to one `Subgraph`, not the whole `Gateway`; callers transform several descriptions explicitly when desired. Build first acquires and validates the original schema and extracts Federation capabilities, then transforms the schema, capability coordinates, and downstream/local translations together before composition and ordinary-lookup validation. All explicit lookup, shareability, requirement, argument-mapping, key, field-set, and correlation names are authored against the transformed gateway-visible schema; fluent method call order does not change that coordinate space. The source adapter uses the compiled inverse mapping for downstream or local execution. A transform may not add an execution source, inject an arbitrary resolver, depend on `R`, inspect request data, change source ownership, or hide explicit identity/lookup requirements. Build preserves original coordinates and rejects an exclusion/rename that leaves any key, lookup, requirement, argument mapping, or Federation capability non-total.

Every cross-source transition requires explicit identity and recall metadata owned by the resolving source. Federation derives this from `@key` and `_entities`. Ordinary/local sources declare keys and lookup fields in Scala. Build parses and validates all selection strings, coordinates, arguments, result shapes, cycles, and correlations.

Single lookup means zero or one entity for one key, though the adapter may pack several instances as aliased root fields. Native batch lookup invokes a list-capable field and declares either ordered correlation or correlation by a returned key selection. Position must never be assumed accidentally.

The v1 lookup strings are intentionally small and build-validated. `entity` and lookup `field` are exact GraphQL names; `requireArguments.field` is a `Type.field` schema coordinate. `key`, `fields`, and `BatchCorrelation.ByKey` are GraphQL field-set selections parsed by Caliban, without aliases, directives, arguments, fragment syntax, or variables. `arguments`/`fromParent` map keys are exact source argument names. An `ArgumentMapping.Parent` value is one leaf path selected by the declared key/parent field set; nested components are separated with `.`. Paths cannot traverse lists and must end at an input-compatible leaf. `ArgumentMapping.InputObject` recursively constructs a source input object from those leaves. The build validates field-set/path existence, input names and nullability, list lifting, defaults, and complete compatibility.

For a single lookup, each mapping builds the corresponding argument value. For a native batch lookup, it is evaluated once per stable deduplicated entity and the values are lifted into the declared list argument. This supports both `[ID!]!` and compound inputs such as `[ProductKey!]!` without a general expression language.

Worked lookup contracts:

```scala
inventory.lookup(
  entity = "Product",
  key = "id",
  field = "productById",
  arguments = ArgumentMapping.paths("id" -> "id")
)
```

For two distinct `Product.id` values, the prepared call may pack aliases but is semantically equivalent to `query($k0: ID!, $k1: ID!) { e0: productById(id: $k0) { id price } e1: productById(id: $k1) { id price } }`. Each alias correlates to its input key; zero/null means that entity is absent.

```scala
inventory.batchLookup(
  entity = "Product",
  key = "organization { id }",
  field = "productsByOrganizationIds",
  arguments = ArgumentMapping.paths("ids" -> "organization.id"),
  correlation = BatchCorrelation.Ordered
)
```

This prepares the equivalent of `query($keys: [ID!]!) { productsByOrganizationIds(ids: $keys) { organization { id } price } }`. `Ordered` requires exactly one list position, possibly null, per deduplicated input in input order; length mismatch is a source protocol failure.

```scala
inventory.batchLookup(
  entity = "Product",
  key = "sku",
  field = "productsBySkus",
  arguments = ArgumentMapping.paths("skus" -> "sku"),
  correlation = BatchCorrelation.ByKey("sku")
)
```

This permits reordered or omitted results. Every non-null returned item must contain one unique `sku`; duplicates, missing correlation keys, or an unexpected key are source protocol failures. Duplicate client entities are deduplicated before the call and fan out after correlation.

For a compound input object:

```scala
inventory.batchLookup(
  entity = "Product",
  key = "organization { id } sku",
  field = "productsByKeys",
  arguments = Map(
    "keys" -> ArgumentMapping.obj(
      "organizationId" -> ArgumentMapping.parent("organization.id"),
      "sku"            -> ArgumentMapping.parent("sku")
    )
  ),
  correlation = BatchCorrelation.ByKey("organization { id } sku")
)
```

The planner constructs one `ProductKey` input per deduplicated entity and sends their list. `requireArguments(field = "Product.shipping", fromParent = ArgumentMapping.paths("country" -> "seller.countryCode"))` maps source argument `country` from the parent leaf path and removes that argument from the client-visible field.

`requireArguments` maps source-visible arguments from parent selections and removes mapped arguments from the client-visible signature. This is distinct from Federation `@requires`, though both normalize into planner requirements.

A field with multiple providers is shareable only when every provider declares the compatible field shareable, matching Federation's symmetric declaration rule. A key field or a field used through an accepted Federation exception follows the corresponding Federation rule; one unilateral ordinary declaration never authorizes another provider.

Build accumulates stable, deterministically sorted, source-attributed `CompositionDiagnostic` values rather than stopping at the first expected error. Any error returns `GatewayBuildError` and no runtime; all successfully acquired sibling resources are released. Caller interruption interrupts build and fabricates no diagnostics. Successful warnings are exposed on the runtime and are not logged automatically.

The result is one immutable in-memory composed graph and integer-indexed routing topology. There is no serialized artifact in this milestone.

## Operation frontend and cache boundaries

Reuse Caliban's existing parser and static validation semantics through the structured Federation MVP. Ticket 15 then adds `Parser.parseQuery(query, ParserLimits(...))` in core so token, nesting, and AST-node budgets are enforced during parsing, before an oversized AST is allocated. Existing parser APIs remain compatible.

Operation pipeline:

1. Read the clock, derive one absolute finite deadline, and enter bounded admission under that deadline; successful admission atomically registers the request active.
2. Enforce the raw supplied-query byte limit and structurally bound request variables and extensions before invoking the installed `OperationResolver`; default resolver behavior requires query text. This protects resolver implementations used by direct embedded callers as well as Quick.
3. Resolve canonical query text, enforce its byte limit, and use the schema-independent parsed-document cache.
4. Parse with bounded Caliban parsing on a miss.
5. Select the operation, run configured validation unless skipped, perform mandatory gateway structural/type checks, normalize fragments/directives/selections, compile variable/argument expressions, and calculate structural inputs.
6. Plan, verify, and cache one complete `PlannedOperation` under the graph generation.
7. On every request, coerce the already structurally bounded variables into request-owned slots, apply request-dependent `OperationPolicy`, and execute.

Never cache Caliban `ExecutionRequest` or another value containing concrete variables. A `PreparedOperation` is selected, statically valid, normalized, schema-aware, and variable-independent. A `PlannedOperation` adds the execution plan, input/result programs, projection metadata, cost, and planning statistics.

Parsed documents contain no schema-derived references, but v1 still owns their cache inside the one runtime generation rather than introducing a cross-generation facility before reload exists. Their key is canonical query-text identity plus parser-limit/version discriminator. Prepared/planned values are generation-owned. Their key includes graph-generation identity, parsed-document identity, selected operation name (including the absent-name case), and every stable validation/visibility/planning discriminator. Runtime variables, headers, trace context, and ordinary request extensions are never cache keys. Any policy affecting visibility, validation, normalization, or planning is fixed at build or contributes an explicit stable discriminator; otherwise that request bypasses the prepared/planned cache.

Caches are finite, weighted, and independently budgeted. Cache hits avoid single-flight coordination. Deterministic parse/validation/planning failures may enter a small weight-and-TTL-bounded negative cache only after admission checks. Oversized valid operations execute without cache admission if they remain within operation/plan safety limits.

Keyed single-flight belongs to the runtime scope. Waiters cancel independently; shared computation survives while another waiter remains and is interrupted when it has no waiters or the runtime closes. Only successful complete values are inserted.

### Client introspection

Client introspection is executed locally from the composed client schema; it is never forwarded to a source. This covers `__schema`, `__type`, and root-object `__typename`. An operation may mix root introspection fields and routed data fields: the planner emits local constant/introspection work beside ordinary source calls, then projection restores client order. Normal validation still governs where meta-fields are legal.

Composition keeps hidden routing metadata separately from the client schema. Federation `@inaccessible` types and fields may remain available for keys, requirements, and downstream operations, but they are absent from client introspection and client validation rejects selecting them. Internal selections never leak into introspection or projected data. Quick's `enableIntrospection = false` rejects client `__schema`/`__type` through the same validation configuration on both structured and encoded paths; internal gateway schema acquisition and internal `__typename` use are unaffected.

## Planning

Use one planner for remote ordinary, remote Federation, and local Caliban sources. It consumes the prepared operation and immutable routing topology, not raw introspection objects or Federation directives.

The planner fixes all source choices, downstream operations, entity transitions, input/output mappings, conditions, merge destinations, and dependencies before execution. Runtime may evaluate variable conditions, batch cardinality, or empty batches; it never searches for a route, fails over, or replans after a source failure.

Maintain two private representations:

- a rich immutable Scala graph using closed ADTs, compact typed IDs, and persistent values for route search, transforms, costing, diagnostics, and verification;
- a verified dense execution DAG with integer node IDs, compact dependency arrays, typed payload tables, and small instruction programs.

Unary v1 needs only `SourceCall` and `Condition` executable nodes. Dependency edges express sequence and parallelism. Merge destinations and input/output programs belong to source-call metadata. Entity extraction, type filtering, key construction, stable deduplication, variable creation, correlation, error-path rewriting, and integration are compiled instruction programs—not arbitrary closures or generated bytecode.

Planner pipeline:

1. Walk the prepared selection against topology.
2. Enumerate feasible routes and recursively satisfy key/requirement obligations while rejecting cycles.
3. Select a candidate through deterministic bounded cost search.
4. Compile source-specific calls plus input/output programs.
5. Coalesce compatible calls, remove redundant transitions/internal selections, and simplify conditions.
6. Verify invariants, recompute final cost, lower to the execution DAG, and retain planning statistics.

The fixed versioned cost heuristic strongly prefers, in order: fewer sequential network stages, fewer source calls, larger compatible batches/less duplicate entity work, less unnecessary downstream data, then stable topology IDs. It does not use observed latency or arbitrary public weights.

If the explored-state limit is reached after finding a feasible plan, use the best plan and mark `searchTruncated`. If no feasible plan was found, report a distinct planning-limit diagnostic. Never cache or execute a partial plan.

Coalesce only calls with the same execution source and operation type, completed dependency frontier, compatible condition/policy/batching context, independently recoverable mappings, and no mutation fence. A shared URL is insufficient.

Keys and requirements are recursive obligations. Abstract selections plan per possible runtime type and insert internal `__typename` when required. Literal conditions fold during normalization; variable conditions become Boolean programs and explicit plan conditions when they can suppress calls.

Independent query roots have no dependency. For mutations, fence the complete routed subtree of each top-level field in document order. Nested work within one mutation root may still run concurrently.

Before caching, verify DAG acyclicity/reachability, projection coverage, input availability, condition dominance, key/requirement/type/capability validity, merge/error paths, mutation ordering, and justification for every internal selection.

Expose only a deterministic semantic `PlanExplanation`: source names, canonical downstream operations, dependencies, transitions, conditions, merge paths, cost, explored states, and truncation. Do not expose topology IDs, executable arrays, prepared operations, or response programs.

## Execution sources and transport

The public API exposes only built-in ordinary remote, Federation remote, and local Caliban constructors. The general connector seam remains private until a real additional protocol forces a complete schema/normalization/planning/execution contract.

Use two phases internally:

- during planning, each source adapter compiles an immutable prepared call;
- during execution, the scheduler supplies runtime inputs and invokes it.

The scheduler does not know about HTTP, `_entities`, or Caliban interpreters.

Remote ordinary and Federation sources share one GraphQL transport. Start with sttp client4 and one scoped pooled JVM backend created, owned, and closed by the gateway runtime. V1 exposes typed global `TransportConfig` for finite connection/read limits plus supported proxy, TLS, protocol, and decompression settings, but no public sttp-backend injection. Per-source configuration may narrow request-level deadlines and limits but cannot select a different backend-level transport profile. Keep the source-transport seam private so deterministic tests can use a fake adapter and a benchmarked backend replacement changes no public interface.

The initial downstream wire contract follows the current [GraphQL-over-HTTP specification](https://graphql.github.io/graphql-over-http/draft/): send unary operations as UTF-8 `POST` JSON with `Content-Type: application/json` and `Accept: application/graphql-response+json, application/json;q=0.9`. The body contains only `query`, optional `operationName`, variables, and explicitly supported extensions. Schema introspection and `_service` acquisition use the same request contract, their static acquisition headers, and their own finite acquisition budget. Redirects are disabled by default for acquisition and execution so credentials cannot silently move to another origin. If explicitly enabled, redirect hops remain finite and authorization/cookie headers are never forwarded across origins.

Response classification is media-type-first. A bounded, well-formed `application/graphql-response+json` GraphQL envelope is a `SourceResult` regardless of HTTP status. A bounded, well-formed `application/json` envelope is trusted as GraphQL only for a 2xx response; a non-2xx legacy JSON response is a transport/protocol failure because it may originate at an intermediary. Missing/unsupported media type, empty/204 response, malformed JSON, invalid GraphQL envelope, decoded-body overflow, or JSON token/nesting overflow is a `SourceFailure`. A valid GraphQL envelope always wins over retryable status classification and is never retried. A non-GraphQL 500/502/503/504 may be retried under the configured query policy. TLS, proxy, decompression, and maximum decoded bytes/structure are explicit gateway transport settings rather than ambient client defaults.

Remote source bodies are bounded after content decoding and may retain raw UTF-8. Each attempt owns/releases its response resource. A valid GraphQL response with partial data and errors is a `SourceResult`; transport failure, invalid protocol data, timeout, or body limit is a typed `SourceFailure`.

Local sources initially execute through the normal Caliban interpreter to preserve wrappers, masking, tracing, and application behavior. Import their `ResponseValue` structurally into the response store without a JSON round trip. Add a generally coherent prepared local-call API to Caliban core only if measurement proves repeated frontend work materially violates the local/mixed budget.

Batch only within one client operation. The planner identifies compatibility; the source adapter packs and correlates `_entities`, aliased single lookups, native batch lookups, or local calls. One logical batch holds one source permit across all retry attempts/backoff. Never batch unrelated client requests in v1.

### Headers

Core's narrow `IncomingHeaders` FiberRef context carries inbound values without depending on a server library. Quick installs it around structured or encoded execution; direct callers may use the explicit runtime overload, whose argument wins over the FiberRef, while inherited interpreter methods use the FiberRef value or empty fallback. Header policy per source may select inbound names, explicitly forward all, add static values, compute effectful values from `R`, and remove values. Reading incoming headers does not widen `R`; only effectful user computations do.

Names compare case-insensitively. Configured values override forwarded values; removals apply last. Standard hop-by-hop headers, every additional header named by an inbound `Connection` value, proxy credentials, and transport-owned `Host`, `Content-Length`, `Content-Type`, `Accept`, `Accept-Encoding`, and trace-propagation headers are always stripped from user policy output. Forward-all is explicit, never default. The transport writes its protocol headers, then validated trace propagation is injected and cannot be overridden by blindly forwarded values. The same reserved-name protection applies to static acquisition headers. Local sources do not inherit HTTP headers or remote retry behavior.

## Execution engine and lifecycle

`build` returns one scoped, concurrently reusable runtime owning one graph generation: composed schema/topology, planner state, source handles, permits, admission state, caches, and gateway-created fibers. Every request owns its coordinator, variables, batches, response store, source documents, and buffers.

The runtime state machine is `Running -> Draining -> Closed`.

`GatewayStatus` reports the state plus admitted, queued, and overdue counts. `overdue` counts admitted requests whose deadline has fired but whose structured request tree has not yet exited. Such a request stays admitted and is never reclassified as completed or detached work; the count makes a drain delayed by uninterruptible user-provided effects distinguishable from healthy in-flight load.

- Admission and active registration are one atomic protocol.
- Drain stops admission, wakes/rejects queued waiters, and waits interruptibly for already admitted operations.
- Concurrent drain callers await the same completion.
- Interrupting a drain waiter does not resume the runtime or cancel active operations.
- Closing the enclosing scope is the only forced-close API. It interrupts active work and runtime-owned background/single-flight fibers and closes gateway-owned transport/resources when their structured users have released them.
- Arbitrary user code cannot be forcibly terminated on the JVM. A user-provided effect that remains uninterruptible—including local Caliban execution, operation resolution, operation policy, or effectful header policy—can delay request completion, graceful drain, and enclosing-scope finalization. The gateway does not detach it, close borrowed services underneath it, or falsely report it as cleaned.

An operation remains active through final projection and conversion into caller-owned bytes or a structured response. Exactly-once finalization decrements active accounting only after request-owned state cannot escape. If a user-provided effect ignores interruption, its environment, inputs, resources, and any acquired source permit remain request-owned until that effect actually exits. Once the deadline has closed result delivery, any eventual result is discarded and released before the timeout response may complete.

Use structured concurrency without an escape hatch: one request scope, one coordinator, and source-call child fibers. Source fibers wait for permits, execute prepared calls, ingest owned results, and transfer completion to the coordinator. They never mutate shared response state. The coordinator alone integrates, advances dependencies, builds batches, applies null completion, cancels unreachable work, and starts newly ready calls. No request child or user callback is reparented or detached after it has observed request inputs or the caller-supplied environment.

Prefer atomic/idempotent ownership over broad uninterruptibility. Mask only short, guaranteed non-suspending state transitions and exactly-once ownership handoffs. Admission waits, source calls, child joins, drain waits, transport shutdown, callbacks, and cleanup remain interruptible or independently bounded where the gateway controls them. Deadline/cancellation first atomically disables result delivery, then interrupts the request tree and retains request ownership until every child has actually exited; only then is response-store/source-document state safe to release.

Concurrency controls:

- one global bounded admission queue/active limit;
- one per-source logical-call permit limit for remote and local sources;
- transport connection/HTTP stream limits remain distinct;
- fairness prevents one request from monopolizing ready work;
- empty/ineligible batches skip without a source call;
- completion order never changes final result ordering.

## Response assembly

Do not use `ResponseValue` as the production remote-routing store. The measured implementation direction is a request-owned hybrid indexed response store.

A remote `SourceDocument` owns bounded UTF-8 buffers plus a plan-driven index. Decode only values required for routing, keys, requirements, type conditions, and nullability into tagged primitive slots. Retain untouched final-output leaves/subtrees as raw byte references when profitable. Local results enter through a structural importer.

The indexed coordinator-owned store is established by the structured Federation MVP in Tickets 7–9; raw-span retention and a packed token index are not MVP requirements. The first correct implementation may decode final leaves into the indexed store behind the same `SourceDocument`/projection seam. Enable retained raw spans only when lifetime tests are complete and profiles show the copy/decoding reduction is worth the complexity. This is not permission to substitute a recursive `ResponseValue` merge engine.

The `ResponseStore` is coordinator-confined. Object/field slots come from the planned operation; dynamic lists use contiguous handles; runtime references are integer IDs into primitive arrays. Internal key/requirement fields stay addressable but are absent from client projection. The verifier rejects conflicting writers.

Compile null completion into the layout. Each slot knows its nullability and next propagation boundary. A null at a non-null position walks parent IDs to the nearest nullable boundary, marks newly unreachable descendants, and allows dependent calls to be skipped/cancelled. Completion is idempotent and never rebuilds a tree.

Store GraphQL errors separately in compact append-only records. Preserve source message, rewritten client path, and allowed extensions; source locations and unapproved extensions are omitted by default. Translate paths through compiled source mappings and entity fan-out. A missing/unusable source path attaches at the call's merge boundary. Error order is deterministic by plan/source ordinal, not completion order.

The projection writer walks the prepared client selection in client order and writes bounded chunks directly. It emits raw spans when safe and encoded materialized values otherwise. It does not build a second JSON tree. Source buffers remain leased until every projection that references them completes; returned encoded bytes are caller-owned and remain valid after the effect.

Begin with plan-specialized jsoniter decoding plus a private raw-reference seam. Add an offset-capable scanner or packed token index only when integrated profiles show retained-subtree copies are material. Pooling is profile-driven; retained buffers/arrays are size-capped and cleared.

## Outcomes, errors, resilience, and policy

### Outcome classes

- `RequestError`: operation resolution, parsing, validation, coercion, policy, or optional structural-limit rejection before execution.
- `Executed`: a valid GraphQL result, including source GraphQL errors, integrated source failures, partial data, and any deadline result that can be completed reliably.
- `GatewayFailure`: overload/drain rejection, inability to produce a trustworthy result before deadline, final response overflow, or masked internal engine failure.

Caller interruption remains ZIO interruption and produces no response.

Only gateway-authored failures receive stable gateway codes. Initial codes:

`GATEWAY_OVERLOADED`, `GATEWAY_DRAINING`, `GATEWAY_TIMEOUT`, `GATEWAY_RESPONSE_TOO_LARGE`, `INTERNAL_SERVER_ERROR`, `SOURCE_REQUEST_ERROR`, `SOURCE_TRANSPORT_ERROR`, `SOURCE_TIMEOUT`, `SOURCE_PROTOCOL_ERROR`, `SOURCE_RESPONSE_TOO_LARGE`, `OPERATION_NOT_FOUND`, `OPERATION_NOT_ALLOWED`, and `LIMIT_EXCEEDED`.

Valid source GraphQL errors retain messages and rewritten paths. Allowlist `extensions.code` by default. Source locations and other extensions are omitted; typed global/per-source configuration may mask messages and change the allowlist. Transport/protocol/timeout/limit failures use safe gateway-authored messages and never expose endpoints, bodies, source names, stack traces, or throwables. Local Caliban errors preserve Caliban masking semantics.

Drop source top-level `extensions` initially. This does not remove allowlisted extensions on individual errors.

### Deadlines and cancellation

One finite configured request timeout is converted to an absolute deadline before admission and includes queue wait, active registration, operation resolution, frontend/cache/single-flight, planning, permit waits, retries, source work, integration, and projection. Per-request options may only shorten it. Per-source timeouts may only narrow it and include source readiness, permit wait, attempts/backoff, response ingestion, protocol validation, and transfer of an owned `SourceResult` to the coordinator. They end before coordinator integration mutates the response store. Every wait receives the remaining duration; budgets never restart.

The runtime owns prebuilt bounded timeout envelopes for both response formats plus an immutable structured equivalent. A request supervisor races the absolute deadline against trustworthy completion. If the deadline wins, it atomically disables result delivery, increments overdue accounting while the request remains active, and interrupts the request tree. Cooperative work joins and releases normally, after which the request returns the prebuilt `GATEWAY_TIMEOUT` without ordinary projection. A user-provided effect that remains uninterruptible stays inside the request scope with its caller-supplied environment, inputs, resources, any acquired permit, and active accounting; it can delay the timeout response until it exits. This rule covers local Caliban execution, operation resolution, operation policy, and effectful header policy. Any eventual result is discarded and can only complete cleanup, never modify response state.

Caller cancellation follows the same structured interruption and ownership protocol but returns no envelope. Scope close interrupts active fibers and runtime-owned work; it may wait for an uninterruptible user-provided effect because releasing the request scope, its environment, or dependent gateway resources underneath that code would be unsafe. Consequently the configured deadline is an interruption deadline and a strict response deadline for gateway-owned and cooperative work, not a promise to kill arbitrary JVM application code or detach it from its scope.

Source timeout becomes `SOURCE_TIMEOUT` at its merge boundary only when it wins before `SourceResult` ownership transfers to the coordinator; independent work continues. Once transferred, integration is governed solely by the request deadline and cannot be reclassified halfway through mutation. A configured retry `maxElapsed` greater than the source timeout is invalid rather than silently misleading; each attempt and backoff is also clipped to the remaining source and request deadlines.

### Retry

Remote retries are disabled by default and configured per source. Only GraphQL query calls are replay-safe. Never retry mutations, local calls, valid GraphQL results, invalid protocol/request data, TLS certificate failures, or engine defects.

Default enabled-policy conditions are connection failure, attempt timeout, and HTTP 500/502/503/504. Use bounded full-jitter exponential backoff with maximum attempts and elapsed retry time. `Retry-After` is honored only inside remaining budgets. One source permit covers attempts and backoff; serialize inputs and evaluate effectful headers once per logical source call, while each attempt gets a fresh transport request/body lease and trace injection.

Circuit breaking, hedging, adaptive routing, and fallback providers are deferred.

### Operation resolver and policy

Install at most one `OperationResolver[-R]` and one `OperationPolicy[-R]`; users compose them explicitly.

Resolver runs after admission and before parsing, returning canonical query text or typed not-found/rejected/unavailable failure. It cannot rewrite variables, operation name, schema, validation, or plans. APQ/manifests/safelists are later implementations of this seam.

Policy runs after planned-operation lookup and variable binding but before dispatch. It sees a stable read-only view of operation type/name, selected client coordinates, preserved security metadata, and only coerced inputs required for its decision. It can allow or reject the entire operation, never rewrite selection, visibility, variables, plan, source choice, or data.

A graph containing composed `@authenticated`, `@requiresScopes`, or `@policy` metadata fails build unless a policy is installed. This is fail-closed; there is no built-in authorization engine.

## Limits, configuration, and observability

Always-finite resource limits cover Quick request-body bytes, supplied/resolved query bytes, parser tokens/nesting/AST nodes, variable/extension nodes/depth, schema-acquisition time/body/schema structure, source body bytes/JSON structure, planner states/nodes, request-owned response memory, final encoded bytes, admission/queue size, concurrency, and cache weights. Callers may configure large finite values but cannot disable these protections. These bounds limit work the gateway admits or owns; no numeric setting can bound how long an arbitrary uninterruptible user-provided effect runs.

Operation depth, alias/root/directive counts, and similar structural restrictions are optional policies. Client-input/operation violations are request errors; source-body overflow is a source failure; final-output overflow is a gateway failure.

Configuration is immutable and frozen at build. Use focused typed case classes for composition, operations, planning, gateway execution, response, transport/acquisition, remote source, local source, header, retry, and source execution settings. Explicit source values replace global defaults. Optional values mean absence only; inheritable/disable-able facilities use the explicit `Inherited | Disabled | Configured(value)` ADT. Independent safety caps resolve to the strictest value. Invalid contradictions fail build; valid ineffective values produce deterministic warnings.

Changing retry, timeout, limits, masking, resolver, or policy requires building a new runtime. Per-request options only narrow. There is no untyped options map or environment-variable reader in the library.

Public observability initially consists of bounded ZIO metrics and safe automatic defect logging. Metric labels are restricted to operation type, source, outcome/code, and cache kind. Record request/in-flight/admission/overdue, planning/cache, source/permit/attempt/bytes/outcome, integration/projection/output, cancellation/deadline requested, user-provided effects still active after interruption, runtime state, and dropped telemetry. Never use raw query, variables, headers, bodies, responses, upstream error messages, operation name, or hashes as metric labels.

Automatically log only unexpected gateway defects and finalizer/resource-release failures with bounded safe annotations. Expected GraphQL/source/request errors, retries, denials, limits, and diagnostics are not logged by the library.

Preserve active ZIO/FiberRef trace context from the first slice. Implement actual OpenTelemetry spans last: one request span, planning span only on cache miss, one logical source-call span covering permit/retries/ingestion, and build/composition spans. Attempts are events or sibling downstream spans, not nested call spans. No span per field/entity/plan node. Inject validated W3C context after header policy; baggage is disabled by default.

## Non-negotiable invariants

1. Build yields a complete immutable graph generation or no runtime and no leaked resources.
2. Request-time environment `R` is never captured during build.
3. Cross-source routing requires explicit identity/lookup metadata.
4. Plans are complete, deterministic, variable-independent, verified, and generation-owned.
5. Runtime never discovers routes or fails over after a source failure.
6. One coordinator owns response integration; source fibers transfer owned results.
7. Request-owned raw references never outlive their source-document leases.
8. Structured and encoded surfaces have equivalent GraphQL semantics.
9. Quick direct encoding has the same method/media/status semantics as its structured path.
10. Valid source GraphQL errors are results; source failures are not confused with them.
11. Caller interruption is never converted into a response or ordinary source failure.
12. Every gateway-owned resource, queue, cache, body, buffer, fiber, deadline, and concurrency path is bounded and scoped. Any user-provided effect that ignores interruption remains request-owned, active, and observable until it really terminates; its request is also overdue once that request's deadline has fired. The gateway never detaches it or releases its environment underneath it.
13. Mutation top-level order and response field order are deterministic regardless of concurrency.
14. No public interface exposes executable plans, routing topology, caches, response store, or low-level transport.

## Canonical executable scenarios

### Federation entity transition

A Products source returns products and keys. A Reviews source resolves those products with `_entities`.

Expected plan and execution:

1. Root Products source call.
2. Integrate products and retain internal key fields.
3. Build a stable deduplicated entity batch with fan-out to every client location.
4. Invoke one prepared Reviews entity call.
5. Correlate results/errors to all original occurrences.
6. Project client fields in requested order, omitting internal keys.

### Mixed remote/local graph

The canonical query fetches remote Products, local-Caliban Pricing, and conditionally included remote Reviews.

Expected behavior:

1. Establish the absolute deadline, admit/register active under it, then resolve/prepare/bind.
2. Evaluate the Reviews condition and execute Products.
3. Integrate Products and form Pricing/Reviews entity inputs.
4. Execute local Pricing and enabled remote Reviews concurrently under distinct permits.
5. Integrate partial Reviews errors and Caliban-local errors with identical completion semantics.
6. Skip/cancel descendants made unreachable by null propagation.
7. Write one encoded or structured result through the same store/projection.

### Ordinary batch lookup

An ordinary source declares a list-capable lookup. Ordered correlation requires one position (possibly null) per input. Key correlation permits reordered and omitted results but requires a unique returned key. Test duplicates, nulls, missing values, and fan-out.

### Mutation ordering

Two top-level mutation fields route to different sources. The full routed subtree for the first completes before the second begins. Nested calls within the current root may be concurrent. Coalescing cannot cross the fence.

### Failure and cancellation

- Partial source GraphQL errors integrate data and rewritten errors.
- Source timeout/failure attaches at the planned merge boundary before result handoff; integration already accepted by the coordinator is governed by the request deadline.
- Request deadline closes result delivery, marks the admitted request overdue, and interrupts request work. Cooperative work releases promptly and returns the prebuilt safe gateway-timeout envelope; any still-uninterruptible user-provided effect remains active and may delay that envelope until it exits.
- Caller interruption or scope close interrupts admission/permit waits, retries, HTTP/local work, operation resolver/policy/header effects, coordinator work, and projection. Cooperative ownership is released; an uninterruptible user-provided effect remains request-scoped and may delay completion, and no response is fabricated for caller/scope interruption.
- Drain racing admission produces exactly one of two outcomes: registered active execution or typed draining rejection.

## Implementation sequence

The files in [`tickets/`](tickets/) are the authoritative work breakdown. The ranges below group them into product milestones; each ticket's `Blocked by` field controls the exact order and safe parallelism within a milestone. No Quick adapter, external audit, benchmark harness, or broad public configuration model is part of the foundation unless its ticket is reached.

### Milestone 0 — Structured Federation MVP (Tickets 1–13)

- Ticket 1 adds only the unpublished module and minimal `Gateway`/`GatewayRuntime` scaffolding needed to compile and establish public/private boundaries. It does not anticipate source, lookup, header, transport, planner, or configuration APIs owned by later tickets.
- Tickets 2–5 normalize pinned schemas, compose a client schema, prepare operations with Caliban's existing parser semantics, and plan one deterministic remote root call.
- Tickets 6–9 classify and execute one bounded remote call, then integrate nested data, null completion, errors, and classified source failure into the indexed store through a sink-parameterized structured projection writer.
- Tickets 10–13 add local client introspection, one Federation entity transition, stable batching/correlation, and the complete structured Products-to-Reviews path with its request deadline, cancellation, trace-context, and ownership protocol.

Exit checks:

- The provisional Scala matrix compiles and publication remains disabled.
- Direct embedded structured execution handles a root call, an entity transition, partial source failure, null completion, and client introspection with deterministic explanation.
- The first projection implementation is sink-parameterized, but no gateway encoded sink or Quick behavior is implemented yet.
- Parser token, nesting, and AST-node budgets are deliberately not an MVP prerequisite; Ticket 15 adds them against a working semantic baseline.
- Cancellation and deadline tests release cooperative work exactly once and never fabricate a response for caller interruption.

### Milestone 1 — Core response proof and remote-boundary hardening (Tickets 14–17)

After the MVP, establish the reusable response-store oracle and independently harden parsing and remote response ownership; none of those three tickets needs to wait for another. Remote ordinary introspection and Federation `_service` acquisition follows the two hardening tickets. The response oracle unblocks the encoded sink early, while hardening/acquisition does not block code-first ordinary or local graph work.

Exit checks:

- Valid MVP schemas and operations preserve their parsing, validation, preparation, and execution semantics after parser limits are enabled.
- The reusable oracle covers nested completion, entity fan-out, partial failure, and varied completion order and is extended by later execution-breadth tickets.
- Every remote response is classified before GraphQL integration, valid GraphQL envelopes win over HTTP status, and malformed/empty/oversized outcomes are typed and bounded.
- Acquired schemas use independent finite cold-build budgets, protected static headers, redirects disabled by default, and accumulated source-attributed diagnostics.

### Milestone 2 — Heterogeneous graphs (Tickets 18–26)

Add ordinary lookups and local Caliban execution directly from code-first pinned schema input; neither waits for remote schema acquisition or parser hardening. As soon as both work, Ticket 26 proves a fixed-route remote Products/local Pricing/remote Reviews graph. Federation shareability/route choice, batch lookups, the core incoming-header context/runtime header policy, and immutable source execution configuration proceed as independent branches according to their real blockers rather than delaying that first mixed execution. Ticket 23 establishes transforms for the coordinate families available in this milestone; later feature tickets extend that same transform compiler when they introduce new coordinate families.

Exit checks:

- Ordinary-only, local-only, Federation-only, and mixed graphs use the same composition, planner, scheduler, coordinator, and response machinery.
- Environment intersections compile and run, local results make no JSON round trip, and invalid metadata accumulates deterministic diagnostics.

### Milestone 3 — Federation breadth and compatibility (Tickets 27–34)

Complete compound/multiple keys, multi-hop routing, recursive requirements and provides, ownership/visibility, abstract selections, aliases/fragments/directives/conditions, and ordered mutations. Most Federation work remains independent of ordinary/local execution; Ticket 28 deliberately converges with the source-neutral requirement and transform models before adding Federation-specific requirement coordinates. Ticket 14's core response-store oracle is already available, and the requirements, abstract-selection, condition, and mutation tickets extend it with their own semantics. Only then integrate the latest reviewed Federation Gateway Audit and close every in-scope case. Ticket 42 later closes the fully configured conditional mixed scenario once both product branches and their operational semantics exist.

Exit checks:

- Every in-scope audit case in the selected revision passes through native code-first composition; only explicitly reviewed staged features are excluded.
- The reference planner/executor and property scenarios cover completion, path mapping, ownership, and varied execution order.
- Ticket 34 re-runs the real planner/executor across the full candidate Scala matrix and records the confirmed versions while publication remains disabled until Ticket 53.

### Milestone 4 — Operational and HTTP completion (Tickets 35–46)

Follow blockers rather than raw ticket number in this milestone. Tickets 35–39 add caches/single-flight, admission/drain/status, overdue/deadline narrowing, retry and masking policy, and operation resolution/policy. Ticket 43 adds only the generic core encoded capability after the structured MVP; Ticket 44 implements the gateway sink after the response-store oracle. Ticket 45 remains deferred until the structured MVP and runtime-header seam exist, so Quick adapter work does not return to the foundation. Ticket 40 audits every finite gateway, encoded-output, and Quick-input bound; Tickets 41–42 close metrics/logging and operational race testing. Ticket 46 connects Quick to the encoded gateway capability and proves structured/encoded HTTP parity before any benchmark uses it.

Exit checks:

- Exactly-once accounting/release holds under races, interruption, deadlines, drain, retry, and uninterruptible user effects.
- Every mandatory resource bound is finite, including Quick request bytes and final encoded bytes; immutable policy and environment intersections are verified.
- Structured and encoded results are semantically equivalent, and Quick matches the reviewed media/method/status matrix without inspecting response bytes.
- No response is fabricated for caller interruption and expected outcomes are not automatically logged.

### Milestone 5 — Performance closure (Tickets 47–50)

Integrate the latest reviewed GraphQL Gateways Benchmark only after the actual Quick encoded path and operational suite are complete. Establish the semantically validated gate, profile the full engine, set measured finite defaults, optimize the dominant actionable seam, and re-run the standing useful-throughput comparison.

Exit checks:

- Correct useful throughput is at least 85% of the leading compared gateway, or maintainers record the narrowly permitted expiring exception after profiles show no actionable dominant seam and all semantic/operational gates remain green.
- Latency, CPU, allocation, GC, and memory are reported, and audit/operational suites remain green after optimization.

### Milestone 6 — Tracing and release readiness (Tickets 51–53)

Add supported OpenTelemetry integration in its dependency-bearing module, compiling examples and migration documentation, then perform the final public API, environment-intersection, MiMa, clean-checkout, and release review. Ticket 52 is also the convergence point for the independent acquisition, batch-lookup, transform, and fixed-route mixed branches, so none can be omitted from release merely because it did not block the Federation audit or operational spine.

Exit checks:

- Trace relationships and downstream W3C propagation pass without raw-data capture.
- Examples for ordinary, Federation, local, and mixed graphs compile.
- Clean-checkout audit, benchmark, operational, cross-build, documentation, and release suites pass.

## Acceptance oracle

### Compatibility

Primary target: the latest Federation Gateway Audit revision selected when the adapter is implemented, committed to `gateway-compatibility/versions.conf`, refreshed by an explicit reviewed commit before release, and recorded exactly in each run manifest.

The gateway-specific adapter code-first composes each suite's subgraphs, following the existing Hot Chocolate adapter pattern. Every in-scope case must pass. Preserve upstream suite/case identities. Small project tests strengthen full errors, HTTP semantics, source-call invariants, ordinary/local/mixed behavior, and lifecycle without creating a competing broad conformance suite.

A case may be excluded only for an explicitly deferred feature or an invalid/ambiguous fixture, with a recorded reason. Assertion failure is not marked or isolated as flaky; retry only classified infrastructure failure.

### Performance

Primary target: the latest GraphQL Gateways Benchmark revision selected when the adapter is implemented, committed to `gateway-compatibility/versions.conf`, refreshed by an explicit reviewed commit before release, and recorded exactly in each run manifest.

Run Caliban Gateway through Quick Adapter's production encoded path. Keep the upstream workload, subgraph implementations, query, and load shape recognizable, but maintain a small gate profile that removes known comparison artifacts: disable cross-request response and in-flight subgraph-request deduplication for every gateway, exclude setup/readiness traffic from measurement, verify every measured response semantically, normalize CPU/memory/logging/readiness settings, and include Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion whenever the current harness supports them. This does not disable required within-operation entity-key deduplication, call coalescing, or plan caching in any gateway. Record any unsupported competitor rather than silently omitting it. The unmodified upstream-default run remains useful informational evidence but is not the release gate.

Useful throughput counts only responses satisfying semantic checks. Caliban Gateway targets at least 85% of the fastest current competitor under that corrected, recorded profile. Report latency, CPU, allocation, GC, and memory, but impose no initial hard p99/memory threshold while the upstream closed-model benchmark does not support a fair fixed-load gate. Falling short blocks release by default. After profiling has exhausted identified seams, maintainers may approve a release only through `gateway-compatibility/performance-exception.md`, recording the exact gap, profiles, rejected remedies, and why further work is disproportionate. The file expires and must be deleted or re-approved at the next external-version refresh; it never lowers the standing target.

Audit and benchmark adapters live in non-published sbt projects modeled after `apollo-compatibility`, with their fixtures/configuration, checked-in upstream version file, and effective gate profile outside the gateway artifact. Path-filtered workflows read that file and run relevant compatibility checks. Pull requests run project tests, the in-scope audit once its adapter exists, protocol/lifecycle checks, and a cheap benchmark smoke only after entity joins work. Dedicated nightly/phase runs report component/in-process regressions. Release candidates refresh external versions through a reviewed version-file change, record the resulting manifest, and enforce the benchmark throughput gate or its explicit exception process. Exact warmup/repetition/hardware settings are calibrated with the harness rather than invented now.

## Principal risks and responses

| Risk | Required containment |
| --- | --- |
| Federation composition breadth | Use audit cases as breadth-first backlog, preserve source coordinates, and never make Apollo JOIN artifacts the internal model. |
| Planner correctness | Rich immutable IR, invariant verifier before lowering, deterministic explanation, and differential/property tests against a small reference. |
| Raw response lifetime | One coordinator, explicit document leases, bounded ownership, interruption/finalizer tests, and structured-result comparison under varied completion orders. |
| Null/error correctness | Compiled completion/path mappings plus property tests for nested objects/lists, aliases, duplicates, partial failures, and abstract types. |
| Quick encoded divergence | Media/method/status parity suite against structured execution before audit/benchmark use. |
| JVM transport performance | Private transport boundary, pooled sttp baseline, early profiling, backend/specialized ingestion only after evidence. |
| Schema acquisition hangs, redirects, or oversized schemas | Gateway-owned transport, redirects off, static protected headers, and independent finite acquisition time/body/parser/schema limits. |
| Local Caliban overhead | Preserve normal interpreter semantics first; add a core prepared-local seam only after measured need. |
| Cancellation/finalization | Scoped fibers, exact ownership states, narrow masking, and injected cancellation at every suspension/handoff. |
| Uninterruptible application code | Atomic late-result disabling, strict request ownership, retained permits/environment/resources/accounting, overdue-work telemetry, and an explicit warning that response, drain, and scope close may wait because the JVM cannot forcibly terminate user code. |
| Cross-build breaks in real engine code | Provisional Ticket 1 matrix, publication disabled, full Ticket 34 re-confirmation, and evidence required before narrowing. |
| Competitive target remains unreachable | Keep 85% as the standing gate; permit only an expiring checked-in maintainer exception backed by profiles and green semantic/operational suites. |
| Premature optimization | Keep reference tests and external gates; compact representations can change only while semantics remain fixed. |

## Definition of implementation-ready

Implementation is ready to start when this document is approved. No additional architecture ticket is required for:

- private class/file/package layout;
- exact finite numeric defaults, which measurements set before release;
- which sttp JVM backend wins measurement;
- whether raw scanning or coordinator internals need further specialization;
- names refined during compilation while preserving the public shape and semantics above.

A new decision is required only if evidence would change a public API, module/dependency direction, ownership/lifecycle invariant, semantic compatibility promise, or accepted scope boundary.

## Rationale index

- [Plan overview](README.md)
- [Domain language](CONTEXT.md)
- [Leading gateway architecture](decisions/01-leading-gateway-architecture.md)
- [Compatibility baseline](decisions/02-compatibility-baseline.md)
- [Performance architecture](decisions/03-performance-architecture.md)
- [Production baseline](decisions/04-production-baseline.md)
- [Module boundaries](decisions/05-module-boundaries.md)
- [Composition contract](decisions/06-composition-and-artifact.md)
- [Operation frontend](decisions/07-operation-front-end.md)
- [Planning model](decisions/08-planning-model.md)
- [Execution engine](decisions/09-execution-engine.md)
- [Response assembly](decisions/10-response-assembly.md)
- [Execution sources](decisions/11-execution-sources.md)
- [Runtime lifecycle](decisions/12-embedded-runtime-lifecycle.md)
- [Public API](decisions/13-public-api-and-configuration.md)
- [Acceptance oracle](decisions/14-acceptance-oracle.md)
- [Operational semantics](decisions/15-operational-semantics.md)
- [Audit and benchmark research](decisions/17-cross-gateway-audit-and-benchmark.md)
- [Final architecture review](decisions/16-architecture-handoff.md)
