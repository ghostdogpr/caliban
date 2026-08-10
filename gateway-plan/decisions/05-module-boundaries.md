# Choose the gateway module boundaries and ownership model

Type: `grilling`
Status: `resolved`
Blocked by: 01, 03, 04

## Question

What deep module boundaries should separate code-first composition, GraphQL front-end work, planning, execution, response assembly, execution sources, and operational policy so that the embedded hot path stays cohesive, Scala APIs remain useful, and future Composite Schemas or new protocols can be added without speculative abstraction?

## Answer

### Embedded-scope amendment

Adopt one sbt artifact now: **`caliban-gateway`**. It contains code-first composition, the gateway engine, stock remote GraphQL execution, local Caliban execution, and stable extension interfaces. Its parsing/validation/normalization pipeline, planner, caches, execution scheduler, response store, projection writer, and optimized representations are logical modules and packages but remain `private[gateway]` implementation.

Earlier router and CLI names are not planned artifacts in this handoff. Their packaging, serialization, and native-image choices must be reconsidered from the implemented and measured embedded model in a separate Wayfinder effort.

Create further sbt artifacts only for an optional integration that adds meaningful dependencies, a different build/runtime target, or a genuinely independent compatibility surface. Internal ownership alone does not justify another published artifact.

The composition-to-runtime seam is an immutable in-memory composed graph. Serialization is not part of this architecture.

The engine owns a small internal connector seam because remote GraphQL and local Caliban genuinely vary there, but v1 publicly exposes only the built-in source constructors. A third-party connector interface would be incomplete unless it covered schema acquisition, normalization, planning, execution, and result mapping together, so publish one only when a real additional source kind requires it. The stock remote GraphQL and local Caliban implementations remain in `caliban-gateway`; the seam does not force separate artifacts. The exact contract is resolved in [Choose the execution-source and transport contract](11-execution-sources.md).

The engine interface is transport-neutral even though its optimized request/result values may preserve UTF-8 documents, raw variables, and response chunks. HTTP listeners, methods, status mapping, TLS, probes, and process behavior stay outside the embedded library.

Dependencies point one way from gateway modules to Caliban core. Gateway needs must not create a dependency from Caliban core back to gateway; a prepared local-execution interface belongs in core only if it is independently coherent. Coarse typed seams exist for request admission, operation resolution, source dispatch, instrumentation, and final-result observation, but arbitrary middleware cannot intercept planner nodes or response-store internals.

Do not publish a standalone planner initially. Expose stable preparation and plan-explanation behavior through the deep gateway-engine interface, and extract a planner artifact only if a second real caller emerges.

Future gRPC and Composite Schemas are architecture fitness tests, not initial implementations: gRPC should add a dependency-bearing execution-source adapter, while Composite Schemas should extend composition. Neither should replace operation admission, execution scheduling, or response assembly.
