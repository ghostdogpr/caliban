# Choose the public embedded Scala API and configuration model

Type: `grilling`
Status: `resolved`
Blocked by: 04, 05, 06, 11, 12

## Question

What should Scala application developers construct, configure, extend, execute, and observe through the embedded code-first API? Produce API and configuration sketches that make ZIO and Caliban integration excellent, preserve the optimized execution path, and avoid exposing unstable internal representations. Standalone configuration, CLI commands, deployment packaging, and artifact loading are out of scope.

## Decisions

### Runtime and composition

- `Gateway.compose(first, rest*)` creates a non-empty, pure, reusable composition description. A collection-oriented alternative reports an empty collection with `Either` rather than constructing an invalid value.
- `Subgraph.graphql`, `Subgraph.federated`, and `Subgraph.caliban` all produce the same contravariant `Subgraph[-R]`; Federation is a capability, not a distinct executor hierarchy.
- `Subgraph[-R]`, `Gateway[-R]`, and `GatewayRuntime[-R]` preserve ZIO environment requirements and allow Scala to infer intersections across local graphs and effectful configuration.
- `build` is scoped and produces a `GatewayRuntime[-R]` that implements `GraphQLInterpreter[R, CalibanError]`. It deliberately does not extend `GraphQL[R]`, whose final interpreter path would repeat the operation frontend and permit unsafe post-build schema transformations.
- Descriptions are opaque, final, immutable values exposed through constructors and copy-style combinators, not public case classes whose representation becomes compatibility surface.
- Schema transformations belong to the composition description and run before `build`; arbitrary post-build schema transformation is unsupported.

An indicative entry point is:

```scala
val gateway = Gateway.compose(
  Subgraph.graphql("products", productsUri),
  Subgraph.federated("reviews", reviewsUri),
  Subgraph.caliban("pricing", pricingApi)
)

ZIO.scoped(gateway.build.flatMap(runtime => /* serve or execute */))
```

### Configuration and source schemas

- `GatewayConfig` is a typed immutable hierarchy of focused case classes. Global defaults and explicit per-subgraph overrides are separate; source overrides win, and contradictory settings fail during `build`. There is no generic map merge or embedded file/environment decoder.
- Description construction is pure and non-throwing. Values such as URIs are typed where the host ecosystem has an appropriate type; schema names and coordinates remain strings. Semantic validation is accumulated into build diagnostics.
- A remote source has a required execution URI and a typed schema-source choice. Ordinary GraphQL defaults to introspection, Federation defaults to `_service { sdl }`, and either may use pinned SDL. Schema acquisition and execution URIs may differ.
- A failed build returns `GatewayBuildError` containing a non-empty, deterministically ordered collection of stable `CompositionDiagnostic` values. A diagnostic has a code, message, source, optional schema coordinate/location, and severity. An underlying cause is retained only for acquisition failures.

### Execution and observation

- The inherited `GraphQLInterpreter.executeRequest(GraphQLRequest)` is the compatibility boundary and materializes `GraphQLResponse[CalibanError]` only there.
- `GatewayRuntime.executeEncoded(GraphQLRequest)` uses the same frontend, caches, plan, execution, and response store but projects directly into a caller-owned encoded response. Its public result distinguishes request rejection from an executed GraphQL result without exposing engine internals.
- The runtime exposes the client schema as a Caliban `Document`, rendered-SDL conveniences, the interpreter's schema checking support, and deterministic semantic plan explanation. It does not expose the composed graph, routing topology, executable plan, caches, or response store.

### Cross-source metadata

- Ordinary remote and local sources declare identity and recall through source-owned `lookup` combinators. The uniform form uses plain schema names, field selections, field names, and argument-to-parent-path mappings as strings; `build` parses and validates them against the acquired schema and accumulates diagnostics. A concise overload covers a one-field key and argument.
- Do not offer Scala-type/member-selection lookup syntax such as `.lookup[Product](_.id)`: generated Scala types do not exist for arbitrary remote sources, and equivalent local and remote capabilities should have one vocabulary.
- A source-local `shareable(entity, fields)` declaration explicitly permits compatible fields to have multiple providers. Federation sources derive the equivalent capability from their directives. Shareability neither selects a preferred provider nor promises runtime failover.
- Every compatible root contribution participates by default. There is no `exposeAtRoot` or graph-mode switch; hiding and renaming use explicit pre-build schema transformation.
- Federation identity, lookup, and ownership metadata is derived from the acquired Federation schema and cannot be shadowed by Scala overrides initially. Corrections use transformed or pinned SDL. The explicit metadata DSL belongs to ordinary and local graphs.

### Headers and transport

- A small gateway-owned `IncomingHeaders` request service carries incoming header values without depending on Tapir or a server implementation. Server adapters populate it, and direct execution provides it when forwarding is configured.
- Per-source immutable header policies support selected forwarding, explicit forward-all, static values, effectful values, and removal. Effectful policy requirements flow into `Subgraph[-R]`. Names compare case-insensitively; configured values override forwarded values; removals apply last; hop-by-hop headers are always stripped.
- Do not expose a low-level remote transport SPI initially. `build` acquires and owns the stock transport from typed portable configuration, while tests and benchmarks use an internal injection seam. A public extension point waits for a second real transport implementation to establish a stable abstraction.

### Lookup and requirement forms

- Ordinary/local sources expose distinct single and native-batch lookup declarations. A single lookup semantically resolves one key to zero or one entity, but its adapter may pack many instances into one aliased GraphQL operation. A native batch lookup calls a list-capable source field.
- Every native batch lookup declares correlation explicitly: key-based correlation supports reordered and omitted results, while ordered correlation requires one result position, possibly null, for every input. Position is never assumed accidentally.
- An entity may have several lookup alternatives, including different simple or compound keys. Repeated declarations need no user-assigned IDs. Composition rejects exact duplicates and validates every route; planning uses its fixed cost model and stable schema-coordinate tie-breakers rather than declaration order.
- `requireArguments(field, fromParent)` maps otherwise source-visible field arguments to parent selections. Mapped arguments are internal and removed from the client-visible field signature; unmapped arguments remain client-supplied. Composition validates paths, types, cycles, and client/internal collisions. These semantics remain distinct from Federation `@requires`, although both normalize into planner requirements.

### Built-in source construction

- `Subgraph.caliban(name, api: GraphQL[R], config)` is the initial local constructor. Scoped `build` reads and validates the code-first schema and constructs one interpreter. Callers apply Caliban wrappers and transformations to the `GraphQL` value before passing it. There is initially no direct-resolver lookup API or schema-plus-prebuilt-interpreter overload.
- Each built-in constructor accepts one focused optional configuration value: ordinary and Federation remotes use `RemoteSubgraphConfig`, while local graphs use `LocalSubgraphConfig`. Both contain a shared `SourceExecutionConfig` only for genuinely shared semantics such as source-call concurrency.
- Graph-level behavior remains in `GatewayConfig`; cross-source metadata remains on `Subgraph[-R]`. A small set of fluent conveniences may copy configuration, but the immutable case-class hierarchy is the complete inspectable form.

### Build, execution, explanation, and lifecycle

- The initial exact build signature is `Gateway[-R]#build: ZIO[Scope, GatewayBuildError, GatewayRuntime[R]]`. Built-in composition and source acquisition need only the runtime scope; request-time `R` is retained by the result and is never captured during build. A future genuine build service gets a separate build environment rather than forcing all of `R` into startup.
- Configuration and pre-build transformation are part of the immutable composition description through `withConfig` and `transform`. `build` is parameterless, so rebuilding one description creates an independent runtime with identical declared semantics.
- Optimized unary execution returns an immutable caller-owned `EncodedGraphQLResponse(bytes: Chunk[Byte], kind: GraphQLResponseKind)`. The operational-semantics ticket owns the final outcome classification. The bytes may wrap the final array without copying and remain valid after the effect.
- `explain(query, operationName)` performs frontend and planning work but no source execution and requires no request environment. It returns a stable semantic `PlanExplanation` case-class/enum tree with source names, calls, dependencies, transitions, conditions, merge paths, cost, and planning statistics, plus deterministic text and JSON renderers. It excludes executable plans, topology IDs, prepared documents, closures, and response programs.
- `GatewayRuntime.drain: UIO[Unit]` atomically stops admission and waits interruptibly for active operations. `GatewayRuntime.status: UIO[GatewayStatus]` returns lifecycle state plus admitted, queued, and overdue-request counts; overdue means the deadline fired but the structured request tree has not exited. There is no public close or force-shutdown method; closing the enclosing `Scope` is the sole forced-close mechanism.

### Naming, endpoints, transformations, and extension boundary

- Replace the non-contractual prototype names with `caliban.gateway.Gateway`, `Subgraph`, `GatewayRuntime`, `GatewayConfig`, and focused related types. Use the ecosystem spelling `Subgraph`, not `SubGraph`, and do not carry pre-release `SuperGraph` aliases.
- Source names remain case-sensitive strings. `build` requires them to be nonblank, control-character-free, and unique exactly as written; it neither imposes GraphQL-name syntax nor silently trims or canonicalizes them.
- Public endpoint values use `sttp.model.Uri`. The gateway already inherits sttp through Caliban's tools/client stack and Caliban's client API already exposes this type. The choice does not require the internal transport to remain sttp-based. `build` requires remote schema and execution endpoints to be absolute HTTP(S) URIs.
- `SchemaSource` is a closed typed choice between introspection, Federation `_service` SDL, a Caliban `Document`, and SDL text. The two acquired forms may specify a different endpoint or default to the execution endpoint. Ordinary and Federation constructors supply their respective acquisition defaults. There is no arbitrary effectful schema loader initially.
- `Gateway.transform(Transformer[Any])` supports Caliban's structural rename/exclude schema and coordinate behavior before composition is finalized. It translates client coordinates back to source coordinates and rejects an invalid or unsatisfiable transformed graph. Environment-requiring transformers, custom resolver-step behavior, post-build transforms, and public per-source transforms are unsupported initially.
- `GatewayConfig` groups typed `CompositionConfig`, `OperationConfig`, `PlanningConfig`, `GatewayExecutionConfig`, and `ResponseConfig` values. Remote, local, and shared source execution configuration remain source-level. Numeric release defaults come from acceptance measurements. There is no untyped options map, reflective config, or environment-variable reader.
- Do not expose a general extension or middleware hook. Initial extension surfaces are local Caliban wrappers, structural gateway transformation, effectful header policy, and the ZIO environment. Later operational work may add narrow named operation-resolution, policy, and telemetry hooks with explicit lifecycle contracts.

## Consolidated API sketch

The following is an API-shape sketch, not a commitment to internal representation:

```scala
package caliban.gateway

import caliban.{ CalibanError, GraphQL, GraphQLInterpreter, GraphQLRequest }
import caliban.parsing.adt.Document
import caliban.transformers.Transformer
import sttp.model.Uri
import zio.{ Chunk, IO, Scope, UIO, URIO, ZIO }

opaque type Subgraph[-R] = /* private description */

object Subgraph {
  def graphql(name: String, endpoint: Uri): Subgraph[Any]
  def graphql[R](name: String, endpoint: Uri, config: RemoteSubgraphConfig[R]): Subgraph[R]

  def federated(name: String, endpoint: Uri): Subgraph[Any]
  def federated[R](name: String, endpoint: Uri, config: RemoteSubgraphConfig[R]): Subgraph[R]

  def caliban[R](name: String, api: GraphQL[R], config: LocalSubgraphConfig = LocalSubgraphConfig.default): Subgraph[R]
}

extension [R](source: Subgraph[R]) {
  def lookup(
    entity: String,
    key: String,
    field: String,
    arguments: Map[String, String]
  ): Subgraph[R]

  def batchLookup(
    entity: String,
    key: String,
    field: String,
    arguments: Map[String, String],
    correlation: BatchCorrelation
  ): Subgraph[R]

  def shareable(entity: String, fields: String): Subgraph[R]
  def requireArguments(field: String, fromParent: Map[String, String]): Subgraph[R]
  def headers[R1](policy: HeaderPolicy[R1]): Subgraph[R & R1]
}

enum BatchCorrelation {
  case Ordered
  case ByKey(selection: String)
}

enum SchemaSource {
  case Introspection(endpoint: Option[Uri] = None)
  case FederationService(endpoint: Option[Uri] = None)
  case Parsed(document: Document)
  case SDL(value: String)
}

opaque type Gateway[-R] = /* private description */

object Gateway {
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R]
  def from[R](sources: Iterable[Subgraph[R]]): Either[GatewayDescriptionError, Gateway[R]]
}

extension [R](gateway: Gateway[R]) {
  def withConfig(config: GatewayConfig): Gateway[R]
  def transform(transformer: Transformer[Any]): Gateway[R]
  def build: ZIO[Scope, GatewayBuildError, GatewayRuntime[R]]
}

trait GatewayRuntime[-R] extends GraphQLInterpreter[R, CalibanError] {
  def schema: Document
  def render: String
  def renderCompact: String

  def executeEncoded(request: GraphQLRequest): URIO[R, EncodedGraphQLResponse]
  def explain(query: String, operationName: Option[String] = None): IO[CalibanError, PlanExplanation]

  def drain: UIO[Unit]
  def status: UIO[GatewayStatus]
}

final case class EncodedGraphQLResponse(bytes: Chunk[Byte], kind: GraphQLResponseKind)
enum GatewayLifecycleState { case Running, Draining, Closed }
final case class GatewayStatus(
  state: GatewayLifecycleState,
  admitted: Int,
  queued: Int,
  overdue: Int
)

final case class GatewayConfig(
  composition: CompositionConfig = CompositionConfig.default,
  operations: OperationConfig = OperationConfig.default,
  planning: PlanningConfig = PlanningConfig.default,
  execution: GatewayExecutionConfig = GatewayExecutionConfig.default,
  response: ResponseConfig = ResponseConfig.default
)
```

`RemoteSubgraphConfig[-R]` contains an optional constructor-default-overriding `SchemaSource`, a `HeaderPolicy[R]`, remote settings, and shared `SourceExecutionConfig`. `LocalSubgraphConfig` contains local and shared execution settings. `HeaderPolicy[-R]` composes static, forwarded, effectful, and removal rules; forwarding introduces the narrow `IncomingHeaders` requirement. All public descriptions are immutable and safe to share.
