# Choose the distributed planning model and algorithm

Type: `grilling`
Status: `resolved`
Blocked by: 01, 02, 03, 06, 07

## Question

What planner inputs, intermediate representation, dependency model, optimization passes, cache boundary, explainability surface, and failure semantics should turn a validated operation plus execution artifact into an executable distributed plan? Demonstrate the design on representative parallel fetch, entity join, `@requires`, abstract type, and mutation-ordering scenarios.

## Answer

Operation planning compiles a complete immutable routing strategy on a planned-operation cache miss. It fixes source choices, downstream operations, entity transitions, input and output mappings, conditions, merge destinations, and dependencies. Runtime execution evaluates only choices already encoded in the plan: request-variable conditions, entity-batch cardinality, and whether a batch is empty. It never searches for another source or replans after a source failure. Adaptive failover would need to become an explicit future plan feature rather than hidden executor behavior.

Use one planner for Federation-enabled GraphQL, ordinary GraphQL, and local Caliban sources. `Gateway.build` compiles the composed graph into an immutable, integer-indexed routing topology containing root entry points, field providers, possible runtime types, keys, lookups, requirements, source capabilities, and diagnostic source locations. Scala opaque IDs such as source, type, field, and lookup IDs prevent accidental mixing while lowering to primitive integers. Planning traverses this topology rather than repeatedly interpreting Caliban introspection objects or Federation/JOIN directives. A source adapter compiles the selected semantic call into Federation `_entities`, a normal GraphQL lookup, local Caliban execution, or a later protocol-specific call form.

Keep two private plan representations. The planner uses a rich immutable Scala graph with enums, opaque IDs, and persistent values for route search, transformations, costing, and diagnostics. A verified result lowers to a flat execution DAG with dense integer node IDs, compact dependency arrays, and typed payload tables. The executable vocabulary for unary v1 has only:

- `SourceCall`, which invokes one prepared call and completes after its result is integrated;
- `Condition`, which evaluates a prepared Boolean expression and enables the selected branch.

Dependency edges express sequence and parallelism, so `Sequence` and `Parallel` nodes are redundant. Merge destinations belong to source-call metadata rather than `Flatten` nodes. Entity extraction, runtime-type filtering, key construction, stable deduplication, variable creation, result correlation, error-path rewriting, and output integration are small immutable instruction programs attached to source calls. Implement them with Scala enums, integer slots, constant tables, compact arrays, and request-owned storage—not arbitrary closures or generated JVM bytecode. Streaming later adds distinct node kinds rather than making unary nodes stream-shaped preemptively.

The compiler pipeline is:

1. Walk the prepared operation against the generation's routing topology.
2. Enumerate feasible routes and recursively satisfy key and requirement obligations, rejecting cycles.
3. Select a candidate dependency graph through deterministic bounded cost search.
4. Compile source calls plus input and output programs.
5. Coalesce compatible calls, remove redundant transitions and internal selections, and simplify conditions.
6. Verify invariants, recompute final cost, lower to the execution DAG, and retain planning statistics for explanation.

Use a fixed, versioned cost heuristic in v1. In priority order it strongly prefers fewer sequential network stages, fewer total source calls, larger compatible batches and less duplicated entity work, less unnecessary downstream data, and finally stable topology IDs as tie-breakers. Do not use observed source latency or expose arbitrary cost weights initially. The deterministic explored-state limit is graph-generation configuration and therefore part of cache identity. When the limit is reached, return the best feasible plan already found and record `searchTruncated`, explored states, the limit, and best cost. If no feasible plan was found, fail with a distinct planning-limit diagnostic rather than claiming the operation is unsatisfiable or selecting an arbitrary route.

Coalesce calls only when they have the same execution source and operation type, the same completed dependency frontier, compatible conditions, lookup/batching and policy contexts, independently recoverable merge/error mappings, and no mutation fence between them. Query roots may combine into one downstream operation; entity calls may combine when their lookup protocol and representation shapes match. Targeting the same URL is not sufficient. Remote GraphQL adapters render and retain canonical downstream operation templates, stable generated aliases and variable definitions during planning. Request execution binds only values and dynamic batches. Local and future source adapters prepare their equivalent call forms at the same seam.

Keys and required fields are recursive planner obligations. The planner inserts internal selections, finds routes that produce them, and makes consumers depend on every producer. Internal fields remain available to response assembly but are absent from the client projection. Abstract fields are planned with possible-runtime-type sets: the planner adds internal `__typename` where routing requires it, chooses a satisfiable key per concrete type, and compiles type-filtered input branches. Identical branches may coalesce. Preparation fails if any possible runtime type cannot satisfy the exposed path.

Fold literal `@skip` and `@include` conditions during normalization. Compile variable conditions into Boolean programs and explicit plan conditions when they can suppress a source call or entity transition; push corresponding directives into downstream GraphQL operations when useful. Condition values do not enter the plan-cache key. Independent query roots have no dependency. For mutations, fence the complete routed subtree of each top-level mutation field in document order. Nested work inside one root mutation field may run concurrently, but the next root mutation cannot begin early. Contiguous fields sent to one source may combine only if exact order and error/path semantics are preserved.

The generation-owned weighted, single-flight warm cache maps operation identity, operation name, and planning-affecting policy/configuration identity directly to `PlannedOperation`. A `PreparedOperation` is the selected, validated, normalized planner input. A `PlannedOperation` adds the execution plan, input programs, projection metadata, cost, and planning statistics. Runtime variables, headers, and request context are never cached. Keep the schema-independent parsed-document cache separate; do not add another independently sized prepared-operation cache until measurements justify it.

Keep both planner representations private. Expose a deterministic semantic plan explanation containing source calls, canonical downstream operations, dependencies, entity transitions, conditions, merge paths, cost, explored-state count, and truncation status. It is versioned independently of the executable layout. Canonical topology IDs, selection ordering, aliases, downstream operations, tie-breaking, and lowered node ordering ensure equivalent normalized inputs produce stable plans. Final response projection separately preserves the client's requested GraphQL field order.

Before caching, verify that the DAG is acyclic and reachable; every client selection has a projection; every input is produced before consumption; conditions dominate guarded calls; entity keys, requirements, runtime types, and capabilities match; merge and error paths are valid; mutation fences preserve document order; and every internal selection is justified. A valid client operation with no satisfiable route after successful graph composition is a planner invariant failure. Cache and execute no partial plan. Unsupported staged features fail explicitly during preparation. Client `__schema` and `__type` introspection bypass distributed planning and execute locally against the composed client schema through Caliban introspection.

Canonical planner fixtures demonstrate the model:

1. **Parallel roots:** `Products.root || Reviews.root`. Independent query roots start together.
2. **Entity join:** `Products { products { id [internal], name } } -> Reviews.entities(Product.id) { reviews }`. The second call gathers and stably deduplicates runtime IDs, retains fan-out correlation, and merges into every original product.
3. **Multi-source requirement:** `Catalog { products { id [internal] } } -> Inventory.entities(Product.id) { weight [internal], size [internal] } -> Shipping.entities(Product.id, weight, size) { estimatedDelivery }`. Recursive requirements become real dependencies and disappear from client projection.
4. **Abstract result:** `Search { results { __typename [internal], Product.id [internal], User.id [internal] } } -> (Reviews.entities(Product inputs only) || Orders.entities(User inputs only))`. Type-filtered batches run concurrently once Search is integrated.
5. **Ordered mutation roots:** complete `Products.updateProduct -> Inventory.lookupUpdatedAvailability`, then begin `Reviews.publishReview -> Users.lookupAuthor`. The entire first top-level field is fenced from the second.

Each fixture must assert semantic explanation, executable-DAG invariants, generated source operations, internal-field projection, and determinism rather than relying only on text snapshots.
