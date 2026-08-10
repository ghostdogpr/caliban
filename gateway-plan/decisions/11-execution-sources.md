# Choose the execution-source and transport contract

Type: `grilling`
Status: `resolved`
Blocked by: 01, 03, 05, 06

## Question

What capabilities and contracts should distinguish logical subgraphs, remote GraphQL transport, and in-process Caliban execution without hiding protocol semantics or coupling the planner to HTTP? Stress-test the boundary with header propagation, variables, uploads, cancellation, batching, local environment requirements, and a hypothetical future non-GraphQL source.

## Answer

Keep the general connector seam internal in v1 and expose only built-in constructors for ordinary remote GraphQL, Federation-enabled remote GraphQL, and in-process Caliban sources. A useful third-party connector would need schema acquisition, source normalization, planning, execution, and result mapping together; publishing a bare executor would be an incomplete and shallow interface. Design and publish that complete interface only when a real additional source such as gRPC requires it.

Separate three roles. A `Subgraph` is a pure logical description with stable identity, schema acquisition, composition capabilities, and execution configuration. `Gateway.build` acquires schemas and creates immutable execution sources once; failure of any source fails the whole build with accumulated diagnostics and releases anything already acquired. A transport is only the protocol mechanism used by a remote execution source. Ordinary and Federation-enabled remotes use the same GraphQL transport; Federation adds schema and lookup capabilities such as `_service` and `_entities`, not another HTTP executor. Equivalent replica endpoints remain a transport concern under one logical source identity.

Use a two-phase internal contract. During plan construction, the source adapter compiles an immutable prepared call containing its downstream operation, variable/input mapping, and result correlation. During request execution, the scheduler supplies only runtime inputs and executes that prepared call. The scheduler does not know about HTTP, Federation `_entities`, or Caliban interpreters. Keep the stock remote transport internal initially, require a long-lived pooled client rather than per-request construction, and select the concrete HTTP stack through measurement. Whether its dependency is supplied in the ZIO environment, explicitly, or through scoped construction remains delegated to the public-lifecycle decision.

Preserve source environment requirements in gateway types rather than hiding them in untyped service locators. Do not introduce a universal source-request context. Like Caliban, use ordinary ZIO environment services, FiberRefs, tracing, interruption, and adapter-provided scoped layers for request-specific application data. Source deadlines and cancellation are applied by the surrounding ZIO execution tree. A remote source may define static or effectful headers from its environment, select individual inbound headers, or explicitly opt into forwarding all headers. Forward-all is never implicit; standard hop-by-hop headers, names nominated by `Connection`, proxy credentials, and transport-owned protocol/trace headers are always stripped, and per-source removal/override remains available. Local sources do not inherit HTTP headers or retry behavior.

Keep source results private and representation-neutral. Remote GraphQL must be able to retain raw UTF-8/chunks, while local Caliban passes values without a JSON round trip; no universal `ResponseValue` requirement may constrain the hot path. A valid GraphQL response containing partial `data` and `errors` is a successful source result. Transport failures, invalid upstream responses, cancellation, limits, and internal failures use the effect error channel. Remote bodies are ingested under a byte limit and released on success, failure, or interruption; the response-assembly decision chooses the exact retained representation.

The plan identifies batch-compatible prepared calls. The request scheduler groups compatible calls only within one client operation, and the source adapter owns protocol-specific packing and correlation for `_entities`, ordinary batch lookups, aliased root fields, or local execution. One batch consumes one per-source execution permit. Do not batch across unrelated client requests in v1. The engine owns configurable per-source concurrency limits for both remote and local sources; the remote transport separately owns connection and HTTP/2 stream limits.

Initially execute local sources through their normal Caliban interpreter to preserve behavior and wrappers. Keep this behind the local adapter, measure the cost of repeated parsing and validation, and add a generally coherent prepared local-call interface to Caliban core only if the performance gate requires it. Gateway code must not leak into Caliban core.

The v1 contract is unary GraphQL-over-HTTP JSON using downstream POST requests. Multipart uploads are rejected with an explicit unsupported-feature error rather than inconsistently supporting only passthrough cases. Streaming and subscriptions later add a distinct source capability and plan-node kind; do not make unary calls return `ZStream` preemptively. Retries remain opt-in, replay-safe, bounded by the original deadline, and disabled by default; mutations are never retried implicitly.
