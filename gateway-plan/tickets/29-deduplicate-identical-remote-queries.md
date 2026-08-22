# 29 — Deduplicate identical in-flight remote queries

**Outcome:** Concurrent identical remote query work can share one in-flight HTTP exchange without sharing mutations or responses
across distinct request contexts.

**Blocked by:** 17 — Complete the GraphQL HTTP boundary; 18 — Add source execution policy; 20 — Add bounded caches, admission,
and concurrency; 28 — Benchmark, profile, and optimize the real gateway

**Status:** complete

## Completion criteria

- [x] `RemoteGraphQLConfig.Execution` exposes one explicit, default-disabled in-flight query-deduplication setting. This remains a
      remote GraphQL transport feature rather than a gateway-wide cache or a public storage interface.
- [x] After request execution headers and the encoded GraphQL body are finalized, concurrent query calls to the same source with the
      same body and effective header multimap share one in-flight HTTP attempt/retry/decode sequence.
- [x] Header names are compared case-insensitively, repeated values retain their order, and identity material containing credentials
      is never exposed through errors, logs, status, or metrics. Equality must not depend on a collision-prone digest alone.
- [x] Different query text, variables, operation names, or effective headers never share work. Mutations never share work, and
      subscriptions remain deferred.
- [x] Deduplication is source-instance and schema-generation scoped, bounded by the existing source-call admission limit, removes its
      entry on every exit, and retains no completed response cache.
- [x] Each caller retains its own request deadline, interruption, completion, error relocation, and response encoding. Interrupting
      one waiter does not cancel shared work still needed by another waiter, and forced runtime shutdown cannot fabricate a response.
- [x] Effectful headers are evaluated for each caller before identity is selected. Header failures and other pre-call failures remain
      caller-local and do not create a shared entry.
- [x] Focused tests cover one downstream call for identical queries; separation by variables, operation name, and headers; mutation
      isolation; shared success and safe failure; interrupted waiters; retry ownership; cleanup; and concurrent entity lookups.
- [x] The Caliban benchmark adapter enables the feature explicitly and reruns the untouched pinned identical-query workload. Results
      also include a distinct-variable or distinct-header scenario and downstream call counts so deduplication is not presented as general
      throughput.

## Verification

The focused gateway suites cover shared success and failure, identity separation, mutation isolation, interrupted waiters, retry
ownership, cleanup, and concurrent entity lookups. The benchmark adapter opts into the default-disabled feature; no gateway-wide cache
or completed-response cache is involved.

At 50 virtual users, adjacent 10-second measurement slices following a 15-second warmup produced these control results with the
pinned, unmodified request driver. Gateway-call totals include the driver's 100 setup calls so downstream-call ratios cover every
request observed by the instrumented subgraphs.

| Source-call identity | Completed iterations | Gateway calls | Downstream calls | Downstream calls per gateway call | Iterations/s |
| --- | ---: | ---: | ---: | ---: | ---: |
| Identical request body and headers | 4,570 | 4,670 | 8,114 | 1.74 | 384.88 |
| Unique harmless header per logical source call | 2,512 | 2,612 | 23,508 | 9.00 | 218.79 |

The unique-header control prevents sharing and restores all nine planned downstream calls per gateway request. The identical-request
workload reduces actual subgraph traffic; it does not skip response completion or retain completed responses. Absolute throughput is
machine-dependent, while the downstream counts directly verify what work was shared.

Keep the implementation at the existing remote-source execution seam. A small private in-flight call table may deepen that module;
do not add a deduplication hierarchy, storage interface, completed-response cache, or alternate executor.
