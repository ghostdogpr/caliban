# Caliban Gateway Implementation Handoff

## Purpose

Build a production-capable embedded GraphQL gateway that feels like Caliban: a small public interface, direct use of Scala and ZIO, and a few deep private modules. It must compose and execute ordinary GraphQL, Federation-enabled GraphQL, and local Caliban graphs without making Federation a graph-wide mode.

The previous implementation proved that extensive up-front internal architecture can delay useful behavior and make the code foreign to the rest of Caliban. This plan therefore fixes product semantics and stable seams while leaving private representation choices to the smallest implementation that satisfies the current ticket.

## Product scope

The first release is an embedded `caliban-gateway` library.

In scope:

- code-first composition;
- pinned SDL or parsed schema input first, followed by remote introspection and Federation `_service` acquisition;
- ordinary remote GraphQL sources;
- Federation-enabled remote GraphQL sources;
- in-process Caliban graphs;
- queries and mutations over unary GraphQL-over-HTTP;
- structured Caliban interpreter compatibility;
- a Quick-based HTTP deployment path;
- compatibility work driven by the Federation Gateway Audit;
- performance work driven by the GraphQL Gateways Benchmark and profiles.

Deferred:

- subscriptions and incremental delivery;
- standalone router, serialized graph package, offline CLI, and hot reload;
- the Composite Schemas specification as a conformance target;
- gRPC, REST, and other remote protocols;
- a plugin marketplace or general authorization product.

The private design must leave room for later source kinds and delivery modes, but current tickets do not implement abstractions solely for them.

## Public direction

Keep the public surface compact:

- `Gateway` is an immutable, reusable code-first description.
- `Gateway.compose` accepts one or more `Subgraph` descriptions.
- `Gateway.build` creates a scoped `GatewayRuntime` or returns accumulated composition diagnostics.
- `GatewayRuntime` is usable where a Caliban `GraphQLInterpreter` is expected.
- `Subgraph` has built-in constructors for ordinary GraphQL, Federation GraphQL, and later local Caliban graphs.
- Remote constructors use sttp's `Uri` value rather than an unvalidated endpoint string while sttp remains the transport baseline.
- Environment requirements remain contravariant and compose through Scala/ZIO environment intersections.

Constructors and representations remain private unless an application genuinely needs them. Public configuration appears only with executable behavior and additional dependency-bearing features use separate sbt modules only when the dependency boundary is real.

## Caliban-first implementation rules

These rules apply to every ticket.

1. Reuse Caliban's parser, validation, operation selection, variable coercion, field collection, values, errors, and interpreter behavior when their semantics fit.
2. Prefer a narrow core seam that exposes an already validated Caliban operation to the gateway over rebuilding Caliban's operation frontend in the gateway.
3. Keep one authoritative representation for a concept. In particular, start with one plan and one result representation.
4. Transform and discard intermediate data when possible. A pipeline stage does not automatically deserve a top-level model retained for the request lifetime.
5. Create a seam when behavior actually varies. Remote GraphQL and local Caliban justify an execution-source seam once both exist; a hypothetical future protocol does not justify another adapter today.
6. Keep helpers local or nested until multiple modules need their interface.
7. Test primarily through `GatewayRuntime`. Test a private algorithm directly only when it is a deep module with meaningful behavior that cannot be diagnosed through the runtime interface.
8. Implement the generality exercised by the current ticket. Later tickets may deepen a module without changing its interface.
9. Treat specialized arrays, byte spans, custom stores, lowered plans, and caches as optimization candidates. Introduce them only behind an existing seam with benchmark or profile evidence from the real gateway.
10. End every ticket with a simplification pass: account for each new top-level type, remove pass-through modules, and verify that names use GraphQL or Caliban vocabulary where available.

These are design constraints, not line-count quotas. A composition algorithm may be large if it hides real GraphQL behavior behind a small interface. Splitting every helper into a file does not make that module deeper.

## Stable private seams

The implementation needs these responsibilities, though it need not create one package or type for each bullet:

### Composition

Input: subgraph descriptions and their schemas.

Output: either accumulated source-attributed diagnostics or one immutable composed graph containing the client schema and routing metadata.

Composition owns cross-subgraph compatibility and Federation metadata. Ordinary GraphQL schema validity should use existing Caliban machinery wherever possible. Add composition rules from concrete supported behavior and audit cases rather than attempting to implement every possible rule in advance.

### Routing

Input: an already parsed and validated client operation plus the composed graph.

Output: one immutable plan describing source work, dependencies, internal requirements, and result destinations.

The first plan may be a small Scala algebra interpreted directly. Explanation, caching, and execution all use that plan. A second lowered representation requires measured evidence that direct interpretation is a material cost.

Unambiguous routing remains a direct deterministic construction. When several valid owners, keys, lookups, bridge sources, or requirement paths can satisfy the same work, planning evaluates a bounded set of candidates and selects the complete route using deterministic structural costs such as downstream call count, dependency depth, and unnecessary internal selections. It does not invent source-latency estimates. Candidate count, expanded work, and planning duration are finite; exhausting a guardrail fails safely before source dispatch. Candidate states are planner-local exploration data, not a second executable plan representation.

### Execution sources

An execution source accepts prepared GraphQL work and returns either a source result or source failure. Remote GraphQL and local Caliban share GraphQL semantics but may keep specialized implementations and result handling. Local results never require a JSON round trip.

### Execution and completion

Execution interprets the plan with ZIO structured concurrency and produces normal GraphQL data and errors. Begin with Caliban `ResponseValue` and existing completion semantics where practical. Internal key and requirement values must remain available to later routes but absent from the client response.

A specialized response representation may replace this implementation after semantic tests exist and profiles show material benefit. It must remain behind the same execution interface and be checked against structured results.

### Transport

The gateway owns pooled remote transport resources. GraphQL-over-HTTP classification distinguishes a valid GraphQL response from transport/protocol failure before execution integration. Every body and response resource has one owner and is released on success, failure, and interruption.

## Semantic requirements

### Composition

- Source names are non-empty, case-sensitive identifiers and are preserved exactly in routing and diagnostics.
- A build returns a complete composed graph or accumulated deterministic diagnostics; no partial runtime escapes.
- Unique compatible roots coexist automatically.
- Cross-source transitions require explicit key and lookup metadata.
- Federation transport fields and types are absent from the client schema.
- Source coordinates and locations are preserved when available for diagnostics.
- Shareable fields require compatible declarations from every sharing Federation subgraph.
- Transformations and visibility rules affect both the client schema and routing metadata consistently.
- Security directives are retained as composed requirements. A graph using them requires an installed operation policy, and that policy receives the effective requirements of each validated operation before source dispatch.
- `@tag` and custom directives selected by `@composeDirective` retain compatible definitions and applications on composed client coordinates.
- Deferred Federation features that affect routing or security, including progressive override labels and contexts, fail composition explicitly instead of degrading to a supported subset.

### Operations

- Parsing, validation, operation selection, fragments, aliases, directives, variable defaults, and coercion follow Caliban/GraphQL semantics.
- Introspection executes against the composed client schema without a remote source call and obeys Caliban's existing `Configurator` introspection setting on every request.
- Query work may run concurrently when dependencies permit.
- Top-level mutation fields execute in client order, including all routed descendants of each field.
- Planning never guesses a join from matching names.
- Equivalent valid routes use stable tie-breaking; ambiguous routing does not depend on map iteration order.

### Results and errors

- Valid source GraphQL responses may contain data, errors, or both.
- Local Caliban errors retain Caliban behavior. Remote GraphQL errors are rewritten to client paths, redact untrusted messages by default, omit source locations, and allowlist the `code` extension by default; operators may opt into additional disclosure globally or per source.
- Source failure attaches at the route's client merge location while independent work may continue.
- Non-null propagation follows GraphQL semantics and is independent of source completion order.
- Client field order is deterministic.
- Internal routing selections are never projected.
- Gateway-authored messages are safe; raw response bodies, headers, variables, and internal diagnostics are not exposed by default.

### GraphQL over HTTP

- Requests are UTF-8 POST JSON with appropriate `Content-Type` and `Accept` headers.
- Classification is media-type and envelope aware. A valid `application/graphql-response+json` envelope wins over HTTP status classification.
- Redirects are disabled by default for schema acquisition and execution.
- Request and response bodies have finite configurable limits before unbounded materialization.
- Header forwarding is opt-in or explicitly configurable; protocol-owned headers cannot be overridden accidentally.
- Retries are conservative, bounded, and limited to replay-safe failures and operations.

### Lifecycle

- A runtime is scoped and safe to share across fibers.
- Gateway-created work remains in structured scopes.
- Caller interruption stays interruption and does not fabricate a GraphQL response.
- Deadline expiry disables late result delivery and interrupts cooperative work.
- The JVM cannot terminate arbitrary uninterruptible user code; such work may delay response completion, drain, or scope close and remains accounted for until it exits.
- Admission, source concurrency, caches, retries, planner search, and all body/parser limits are finite.

## Performance strategy

Correctness and a simple executable path come first, but performance is a release requirement rather than an afterthought.

- Keep the transport pooled and avoid obvious repeated parsing or serialization.
- Establish semantic end-to-end tests before replacing values or plans with specialized representations.
- Measure plan quality separately from planner CPU cost: downstream calls, dependency depth, and avoidable internal selections matter even when planning is cached.
- Integrate the real Quick path before measuring router throughput.
- Use profiles to locate dominant CPU and allocation costs.
- Optimize the dominant measured seam behind its existing interface.
- Compare optimized and structured implementations with the same semantic corpus.

The response-assembly and operation-front-end prototypes remain useful evidence. They do not require the production implementation to adopt their types or sequencing.

The release target is useful throughput within roughly 15% of the fastest correctly configured supported gateway in the current GraphQL Gateways Benchmark. Record the selected upstream revision, environment, configuration, correctness checks, latency, allocation, GC, and memory. An evidence-backed, expiring maintainer exception is the only release escape hatch.

## Compatibility strategy

Published GraphQL, GraphQL-over-HTTP, and Federation specifications are primary. For ambiguous gateway behavior, inspect current Apollo Router, Hive Router, Cosmo Router, and Hot Chocolate Fusion behavior and bias toward the majority unless it conflicts with a specification or Caliban's public semantics.

After one Federation join works, integrate the current Federation Gateway Audit. Use its failing cases to drive breadth tickets. Preserve upstream test identities and document only genuinely deferred or invalid cases; assertion failures are not marked flaky.

## Testing strategy

Each product ticket adds at least one end-to-end test through `GatewayRuntime` that would fail before the ticket and demonstrates its user-visible result. Prefer small in-process stub servers and local Caliban graphs.

Maintain focused tests for:

- composition diagnostics where several failures must accumulate;
- remote protocol classification and resource release;
- entity correlation, null completion, and path rewriting;
- ambiguous-route quality and deterministic planner guardrails;
- cancellation and ownership races;
- structured versus encoded semantic equality;
- compatibility cases and benchmark response validation.

Avoid golden tests of private representations unless the public feature is a plan explanation. Refactoring a private plan or helper should not require rewriting unrelated tests.

## Ticket execution

The numbered files in [tickets](tickets/) are the authoritative sequence. Blockers express ordering; tickets without a dependency relationship may proceed independently.

An agent working on a ticket must:

1. read this handoff and the ticket;
2. inspect the current Caliban implementation for reusable behavior before designing new models;
3. implement only the ticket's observable outcome and required supporting behavior;
4. test through the stable interface in proportion to the change;
5. perform the simplification pass from the Caliban-first rules;
6. update the ticket status and summarize any intentionally introduced seam with its two current uses.

If satisfying a ticket appears to require a new cross-cutting architecture, stop and revise the plan with the maintainer rather than silently establishing it in code.
