# Caliban Gateway Domain

The shared language for composing and running heterogeneous graphs with Caliban Gateway.

## Language

**Graph**:
The client-visible GraphQL contract handled by the product. A graph may combine ordinary GraphQL, Federation-enabled, and in-process Caliban sources.

**Standalone graph**:
A graph backed by exactly one ordinary GraphQL execution source and requiring no cross-source metadata.
_Avoid_: Non-federated supergraph

**Federated graph**:
A graph containing cross-source entity identity and recall semantics. Its sources may use Federation `_entities` or explicitly declared ordinary/local lookups.
_Avoid_: Federation-only graph

**Subgraph**:
A logically identified GraphQL schema contribution, its composition capabilities, and the configuration needed to construct its execution source. A subgraph is not an endpoint or connection pool.
_Avoid_: Service, endpoint

**Composition**:
Acquiring, validating, normalizing, merging, and satisfiability-checking source schemas to create an executable composed graph. Embedded composition occurs during `build`; later tooling may perform it offline.
_Avoid_: Query planning

**Composition description**:
The pure code-first value that declares subgraphs, explicit cross-source metadata, graph transformations, and build configuration before composition runs.
_Avoid_: Composed graph, gateway runtime, mutable builder

**Composed graph**:
The immutable in-memory result of successful composition, containing the client schema, source ownership, and cross-source execution metadata.
_Avoid_: Serialized artifact, query plan

**Composition diagnostic**:
A stable, source-attributed problem or warning discovered while acquiring, normalizing, merging, or checking the satisfiability of a composition description.
_Avoid_: GraphQL execution error, source failure, thrown exception

**Routing topology**:
The immutable graph-generation view of root entry points, field providers, runtime types, keys, lookups, requirements, and source capabilities used by operation planning.
_Avoid_: Client schema, execution plan, live service registry

**Lookup**:
An explicit capability of a source to recall an entity by stable key, either through Federation `_entities`, a normal GraphQL query field, or an in-process Caliban resolver.
_Avoid_: Join inferred from matching names

**Batch lookup**:
A source capability that recalls several entities through one list-shaped source field and declares how results correlate to requested keys.
_Avoid_: Automatically aliased single lookups, cross-request batching

**Argument mapping**:
A declarative mapping from parent/key leaves into one source-field argument, including compound input objects; batch lookup lifts one mapped value per deduplicated entity into the source list argument.
_Avoid_: Arbitrary expression, resolver, argument rename

**Shareable field**:
A field explicitly permitted to have more than one compatible execution source. Shareability enables deterministic provider selection during planning but does not promise failover.
_Avoid_: Duplicate field, replicated endpoint

**Entity transition**:
A planned movement from values available at one source into an explicit lookup at another source, using a declared key and any required fields.
_Avoid_: Type-name join, implicit stitching

**Entity batch**:
The unique runtime representations gathered for one entity transition, together with correlation back to every original response location. An entity batch exists only within one client operation.
_Avoid_: Cross-request batching, list of HTTP requests

**Requirement**:
Data a source call needs from its parent value in addition to the client's visible selection, including key fields and explicitly declared required fields. Requirements become planner obligations and are not exposed in the final client result.
_Avoid_: Federation `@requires` only, incidental overfetch

**Execution source**:
A built runtime target capable of resolving part of an operation for one subgraph. Initial execution kinds are remote GraphQL and in-process Caliban; Federation changes GraphQL capabilities rather than defining another transport.
_Avoid_: Data source, backend

**Transport**:
The protocol mechanism an execution source uses to perform a remote call. It owns connections and wire behavior but not schema ownership, composition semantics, or source identity.
_Avoid_: Subgraph, execution source

**Header policy**:
A source-owned rule that derives outbound request headers from configured, incoming, and effectful values while enforcing protocol exclusions and precedence.
_Avoid_: Universal request context, transport implementation

**Graph generation**:
An immutable runtime realization of one composed graph and its schema-dependent state. A request belongs to exactly one graph generation.
_Avoid_: Current schema, mutable graph

**Gateway runtime**:
The scoped, concurrently reusable result of building a composition description. It owns one graph generation together with its execution-source handles, caches, limits, and gateway-created fibers.
_Avoid_: Composition description, request coordinator, GraphQL request

**Overdue request**:
An admitted request whose deadline has fired but whose structured request tree has not exited. Late result delivery is disabled, but any uninterruptible user-provided effect—including local Caliban execution, operation resolution, operation policy, or effectful header policy—remains with its caller-supplied environment, acquired permits/resources, and request accounting until it exits.
_Avoid_: Detached work, completed request, leaked fiber

**Execution plan**:
A complete immutable routing strategy for one prepared operation in one graph generation. It fixes source calls, entity transitions, mappings, merge destinations, conditions, and dependencies; execution does not discover alternative routes.
_Avoid_: Query, composed graph, runtime routing policy

**Source call**:
One execution-plan step that invokes a prepared operation against one execution source. Its input and output mappings are part of the plan even though the source adapter owns the concrete protocol call.
_Avoid_: HTTP request, resolver, subgraph

**Source result**:
The valid GraphQL outcome of one source call, including any partial data and GraphQL errors available for integration.
_Avoid_: Successful data only, transport response

**Source failure**:
A typed inability to obtain or accept a valid source result, such as transport failure, invalid protocol data, or a source-body limit violation.
_Avoid_: GraphQL errors in a valid source result, engine defect

**Source permit**:
One slot in an execution source's engine-level concurrency limit, held by one logical source call rather than by an individual transport attempt or connection.
_Avoid_: HTTP connection, HTTP/2 stream, retry attempt

**Prepared operation**:
A selected, statically validated, normalized, and variable-independent client operation for one graph generation. It is the schema-aware input to operation planning.
_Avoid_: Parsed document, Caliban `ExecutionRequest`, planned operation

**Planned operation**:
The complete variable-independent unit cached for warm execution, containing a prepared operation, its execution plan, input programs, projection metadata, cost, and planning statistics.
_Avoid_: Prepared operation, bound request

**Operation planning**:
Selecting and compiling a complete execution plan from a prepared operation and a composed graph. Planning may compare alternative source routes but performs no source calls.
_Avoid_: Composition, execution

**Plan explanation**:
A deterministic semantic description of an execution plan's source calls, dependencies, transitions, conditions, merge paths, and planning cost. It is not the executable plan's internal representation.
_Avoid_: Serialized execution plan, debug dump

**Execution coordinator**:
The request-local owner that advances one execution plan, starts ready source calls, integrates their results, and completes or skips dependent work. Source calls do not mutate shared response state themselves.
_Avoid_: Query planner, execution source, detached worker

**Source document**:
The request-scoped, bounded representation of one remote source result's UTF-8 buffers and plan-driven value index. Raw references may not outlive its ownership lease.
_Avoid_: Generic JSON DOM, composed response, serialized graph package

**Response store**:
The execution-coordinator-owned indexed state that assembles source values, null completion, concrete output locations, and internal requirement data for one operation. It is not a public response value.
_Avoid_: `ResponseValue`, source document, response cache

**Projection writer**:
The compiled final pass that walks the client's prepared selection in deterministic order and emits one response or delivery-group view from the response store.
_Avoid_: Generic JSON serializer, merge algorithm, source encoder

**Gateway engine**:
The execution component of a gateway runtime that admits GraphQL operations and resolves them against its graph generation.
_Avoid_: Gateway runtime, request coordinator, HTTP server

**Gateway failure**:
A gateway-authored inability to produce a trustworthy ordinary execution result, such as admission unavailability, pre-result deadline expiry, or an internal engine failure.
_Avoid_: Request error, source failure, caller interruption

**Operation resolution**:
The request stage that turns supplied query text or an operation identifier into the canonical GraphQL document text consumed by the operation frontend.
_Avoid_: Parsing, persisted-operation storage, operation planning

**Operation policy**:
A gateway decision boundary that allows or rejects a client operation using its composed security metadata and request inputs without rewriting the operation or its plan.
_Avoid_: Resolver authorization, schema filtering, general middleware

**Encoded response**:
A caller-owned, wire-ready result projected directly from a gateway execution without first constructing Caliban's generic response-value tree. It preserves the same GraphQL response semantics as the compatibility API.
_Avoid_: Source result, response store, serialized graph package

**Acceptance oracle**:
The versioned hierarchy of specifications, project decisions, executable cases, and controlled measurements that determines whether a gateway release is semantically and operationally acceptable.
_Avoid_: Current competitor behavior, benchmark leaderboard

**Useful throughput**:
The measured rate of responses that satisfy the workload's semantic assertions and were not unexpectedly rejected. Fast incorrect responses do not contribute.
_Avoid_: Raw HTTP completion rate, offered request rate

**Competitive workload**:
A deterministic remote-GraphQL scenario from the pinned GraphQL Gateways Benchmark that every compared gateway can execute with equivalent semantics and configuration, and to which the cross-gateway performance gate applies.
_Avoid_: Caliban-only microbenchmark, arbitrary example query
