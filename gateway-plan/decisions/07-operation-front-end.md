# Choose the operation parsing, validation, and normalization strategy

Type: `prototype`
Status: `resolved`
Blocked by: 02, 03, 05

## Question

What representation and pipeline should parse, validate, normalize, authorize extension hooks, and cache client operations while preserving GraphQL semantics and minimizing allocations? Prototype the credible Caliban-reuse and specialized alternatives against representative operations so the choice is evidence-backed.

This ticket requires an executable Scala/JMH-style measurement spike, not an interactive prototype.

## Answer

The executable spike and exact measurements are recorded in [the operation-front-end prototype](../prototypes/07-operation-front-end/README.md). On a valid representative operation, merely caching Caliban's parsed `Document` improved throughput about 1.5 times and reduced allocation 25%, because validation still allocated 15.8 KB per request. Reusing a statically validated document improved throughput about 4.7 times and reduced allocation 74%, leaving variable coercion and field materialization as the warm-path work.

Keep Caliban's parser as the initial implementation. In the existing full-introspection JMH workload under Scala 3, it delivered about eight times the throughput and one eighth the allocation of the available `gql` Scala parser. A purpose-built UTF-8/compact parser remains possible behind a private seam, but it is not justified unless gateway cache-miss or persisted-operation-ingestion profiles later fail the performance gate.

Do not cache Caliban's `ExecutionRequest`: its `Field` tree contains concrete variable values. Introduce a private, immutable, variable-independent gateway `PreparedOperation` instead. It owns the selected operation, operation type, statically validated and normalized selection representation, compiled variable and argument expressions, and precomputed depth/cost inputs. Operation planning combines it with the plan and projection metadata into the generation-owned `PlannedOperation` warm-cache value. Use compact integer indexes and variable slots where measurements justify them; runtime variable values are coerced/bound into request-owned storage.

The operation front end has these stages:

1. Resolve operation text or a future persisted-operation identifier and apply document byte/token/depth admission limits before expensive work.
2. Look up the schema-independent parsed-document cache; on a miss, parse with Caliban and retain the exact source mapping needed for GraphQL errors.
3. Look up or single-flight the generation-owned planned-operation cache using operation identity, operation name, and an explicit discriminator for any behavior that affects validation, normalization, authorization filtering, or planning.
4. On a prepared miss, select the operation, run Caliban-compatible static validation, normalize fragments/directives/selections, compile variable/argument programs and limits, then plan. The planning ticket decides the exact plan representation.
5. On every request, coerce variables and bind them to prepared slots, run request-dependent policy checks, and execute. Variables, headers, trace state, and ordinary request extensions are not cache keys and never enter a cached value.

Use bounded weighted caches. Parsed documents may be process-wide because they have no schema-derived references. Prepared operations and plans belong to one immutable graph generation and are discarded with it. A generation may carry forward hot operation identities for re-preparation, but it may not reuse an old prepared value without proving complete generation and option identity. Cache hits must avoid the single-flight lock and coordination allocation. Cache deterministic parse/validation failures only after admission limits, in a small bounded rejection cache with a short lifetime; never let arbitrary invalid documents occupy the successful-operation budget.

Keep extensions coarse. Operation resolution and pre-analysis admission may reject or replace the input. A policy that changes validation, visibility, normalization, or planning must be installed at build time or provide a stable cache discriminator. Request-dependent authorization checks consume the prepared portion of the planned operation after lookup and may reject it, but cannot mutate cached selections or plans. Do not expose middleware between parser, validator, normalizer, and planner stages.

This follows the competitor majority: all four cache prepared/schema-sensitive operation work with the graph generation; Hive and Fusion additionally separate schema-independent parsed documents; all four avoid planning every warm request. It also leaves one private `OperationFrontend` seam through which Caliban validation or variable coercion can later be specialized without changing the embedded API.
