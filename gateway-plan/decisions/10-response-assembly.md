# Choose the response representation and assembly algorithm

Type: `prototype`
Status: `resolved`
Blocked by: 02, 03, 08, 09

## Question

What internal response representation and merge algorithm should assemble subgraph and local results, preserve GraphQL null propagation and error paths, avoid unnecessary JSON trees or copies, and support future incremental payloads? Benchmark concrete alternatives under join-heavy and error-heavy workloads before selecting one.

This ticket requires an executable Scala/JMH-style measurement spike, not an interactive prototype.

## Evidence and recommended answer

The executable spike and reproducible results are in [Response-assembly measurement spike](../prototypes/10-response-assembly/README.md). It measures parse, federated join, null propagation, projection, error-envelope handling, and final encoding together. At 128 joined products, the plan-specialized indexed path is 5.25x the `ResponseValue` throughput with 72% fewer allocated bytes on the join-heavy workload, and 4.16x with 71% fewer allocated bytes on the error-heavy workload. A primitive raw-slice index lowers allocation by a further 37–39% but is 18–19% slower in this small fixture. A short 512-product run preserves the ordering: the raw index comes within 9% of the indexed path on the large join and allocates 42% less. Setup checks semantic equality of every candidate output.

Select a **request-owned hybrid indexed response store**, not Caliban `ResponseValue` and not an all-purpose raw JSON DOM. A remote `SourceResult` owns a `SourceDocument`: bounded UTF-8 source buffers plus a plan-driven index. The compiled source-result program decodes only values needed for routing, entity input, type conditions, and nullability into tagged primitive slots. It retains untouched client-output leaves and subtrees as raw references `(documentId, start, end)`. Local Caliban results enter the same store through a structural `ResponseValue` importer without a JSON round trip. `ResponseValue` remains the local/public compatibility representation, not the internal remote assembly representation.

The `ResponseStore` is confined to the execution coordinator. Its object shapes and field slots come from the `PlannedOperation`; dynamic lists use contiguous handle ranges, and all runtime references are integer IDs into primitive arrays. Input and output programs address slots by ID, entity batches retain stable source-ordinal-to-output-location fan-out, and integration replaces or fills handles rather than rebuilding immutable trees. The plan verifier rejects conflicting writers. Fields fetched only for keys or requirements remain addressable to later input programs but are absent from the client projection.

Compile null completion into the response layout. Each slot template records its type/nullability and next propagation boundary. Integrating a null for a non-null position follows parent IDs until the nearest nullable position is marked null; list items carry their concrete parent/index handle. The operation is idempotent, marks newly unreachable descendants, and lets the coordinator skip or cancel source calls that can no longer contribute. It does not scan or rebuild a response tree.

Store GraphQL errors separately as compact append-only records. Preserve the source message, locations, and allowed extensions; translate source paths through the source call's compiled output mapping and the entity fan-out table into concrete client response locations. Aliases come from the client projection, not source field names. A source error with unusable or missing path attaches at the source call's merge boundary. Final ordering is deterministic by plan/source ordinal, independent of completion order, while every duplicate entity occurrence may receive the required rewritten path.

The final projection writer walks the prepared client selection, not insertion order. It writes GraphQL response bytes directly to bounded growable chunks, emitting raw spans when possible and encoded primitive/materialized values otherwise; it never constructs a second JSON tree. The exact public gateway-versus-Caliban execution surfaces are left to the public API ticket, but the standard Caliban-compatible surface may materialize a final `GraphQLResponse` only at that boundary. The reference HTTP performance path must retain access to the direct writer, because forcing it through `ResponseValue` would discard the measured benefit.

Source buffers stay owned by the request until the last projection that can reference them is written, and the response-byte budget includes retained input buffers, store growth, errors, and output buffering. Pooling is a later profile-driven optimization; retained arrays/chunks must be size-capped and cleared. For future incremental delivery, response locations and errors also carry a delivery-group ID, and projection is a view over one group rather than destructive extraction. Unary v1 uses only the initial group. Later patch writers may retain/release source-document leases per group without changing merge or null-propagation semantics.

The prototype does not justify forking jsoniter immediately. Begin with plan-specialized jsoniter decoding for materialized values and a private raw-reference seam. Its public `readRawValAsBytes` copies raw values, so add or build an offset-capable UTF-8 scanner only when the gateway pipeline profile confirms retained-subtree allocation is material. Keep the packed token-index alternative behind the store/parser seam and compare it again on escaped strings, wide/deep objects, aliases, one-megabyte payloads, and entity-error path rewriting before promotion.
