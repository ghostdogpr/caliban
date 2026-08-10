# Choose the execution engine and concurrency semantics

Type: `grilling`
Status: `resolved`
Blocked by: 03, 08, 11

## Question

How should ZIO scopes, fibers, interruption, batching, concurrency limits, deadlines, retries, and backpressure execute a distributed plan while preserving GraphQL and mutation ordering semantics? Produce a concrete execution sketch that covers remote and local sources, partial failure, and request cancellation.

## Answer

Execute each planned operation with a compact request-local DAG coordinator inside one ZIO scope. Request-owned primitive arrays track remaining dependencies and node states; a ready queue identifies executable nodes. Only asynchronous `SourceCall` nodes run in child fibers. Conditions, input programs, output integration, dependency updates, and ready-queue work execute directly in the coordinator, with large pure scans chunked by measured thresholds and separated by cooperative `ZIO.yieldNow` boundaries. Never create a fiber per entity, mapping instruction, merge, or other small pure step.

Source-call fibers never mutate the response store. Each produces a private source result or typed failure and transfers it through a request-local completion queue. The coordinator is the sole response-store owner: it integrates completions as they arrive, marks nodes terminal, decrements dependents, and launches newly ready calls. Arrival-order integration releases critical-path work promptly. The plan verifier prevents conflicting writers, while final projection restores deterministic client field order and stable path/node error ordering. Telemetry may retain actual completion timing.

The coordinator remains plan-driven rather than GraphQL-aware. A ready `Condition` evaluates its prepared Boolean expression and skips the disabled branch; mutation root seriality is represented entirely by dependency fences; query parallelism is the absence of dependencies; an empty entity batch skips its source call. Remote and local calls follow the same scheduling lifecycle. The local adapter invokes the normal Caliban interpreter initially, inherits the request ZIO environment, FiberRefs, tracing, and interruption, avoids HTTP/JSON conversion, and preserves Caliban wrappers and ZQuery batching within that graph. The gateway does not combine ZQuery execution across source calls.

A node becomes schedulable when all dependencies are terminal, not only when all succeeded. Its input program examines integrated data and upstream outcomes, executes for every parent item whose keys and requirements remain available, skips ineligible items, and marks the whole call skipped when no eligible inputs remain. Compact request state distinguishes at least pending, running, succeeded, source-failed, and skipped. Do not propagate a single node-level failed bit through an entire descendant DAG; entity eligibility is item-specific.

Normalize source-call outcomes at their boundary. A valid GraphQL response containing partial `data` and `errors` is a successful source result: integrate usable data and rewritten errors, then allow eligible dependent work to continue. A transport, protocol, source-body-limit, or other modeled source failure becomes a source-scoped GraphQL execution error at that call's merge boundary; independent branches continue. Response nullability can make paths unreachable, at which point the coordinator skips or cancels work that can no longer contribute.

Match Caliban's documented effect-failure behavior for source-call user/source execution. Preserve an explicit `ExecutionError`, adding client path and location when absent. Convert another effect failure or defect to the masked message `Effect failure`, retain the original throwable internally for logging and wrappers, and let normal GraphQL nullability determine propagation. Local Caliban already applies this rule. Interruption remains interruption. A defect in the gateway's own coordinator, mapping interpreter, plan invariants, or response-store internals is request-fatal because partial state is no longer trustworthy: retain the full `Cause` diagnostically, interrupt siblings, release resources, and expose only a masked internal error if the caller remains connected. Never collapse typed failure, defect, and interruption through a blanket `catchAllCause`.

Distinguish three forms of stopping:

- Caller interruption or client disconnect interrupts the entire request scope, releases everything, and fabricates no response.
- A configured execution deadline atomically disables late result delivery, marks the admitted request overdue, and interrupts remaining work. Cooperative work then yields a typed deadline outcome from which a connected host can produce a well-formed GraphQL error response. Any uninterruptible user-provided effect remains scoped to the request and can delay that outcome until it exits.
- Semantic early completion, such as null propagation making all remaining output unreachable, interrupts unnecessary children and returns the valid accumulated response; it is not cancellation.

All source fibers are scoped children; no detached work survives the request. Every permit, response body, queued result, and owned buffer uses scoped acquisition/finalization. Prefer atomic/idempotent ownership protocols; if interruption masking is unavoidable, restrict it to a few guaranteed non-suspending instructions that flip ownership or release state. No potentially suspending effect—including a queue wait—may enter a masked region. Permit waits, retry backoff, transports, body ingestion, local execution, mapping chunks, user hooks, and coordinator waits remain interruptible. Cancellation racing completion must leave exactly one owner responsible for each result. A queued source result belongs to the request scope; integration adopts its buffers or releases them, while scope finalization releases every unintegrated result.

Use three distinct backpressure layers:

1. bounded FIFO engine admission with configurable active-operation and queued-operation limits plus optional queue timeout; queue waiting consumes the request deadline, cancellation removes the waiter, and a full queue produces a typed overload outcome;
2. a configurable per-source execution limit shared across all requests and applied equally to remote and local sources, where one dynamic entity batch consumes one source permit;
3. hard plan-node and ready-width limits that bound child fibers for each admitted operation.

Do not add a separate per-request semaphore initially because it can create head-of-line blocking between independent sources. Ready source-call fibers may suspend on their source permit; plan-size and active-operation bounds cap them. HTTP connection and HTTP/2 stream pools remain separate transport limits. A source permit covers one logical call from dispatch through its complete retry sequence and source-result acquisition. Release it after the fully ingested/validated result is transferred to the completion queue, or after failure/cancellation, rather than waiting for response-store integration. Request-level response-byte and plan bounds constrain the completion backlog.

The request deadline begins at engine invocation and includes admission queueing, operation resolution, cache/single-flight waits, variable binding, permit waits, retries, integration, and final projection. A source-call timeout begins when its node becomes ready and covers the source-permit wait, all attempts/backoff, body ingestion, protocol validation, and transfer of an owned source result to the coordinator; it ends before coordinator integration. Its effective deadline is the earlier of the configured source timeout and remaining request deadline. Connect, TLS, response-header, and idle-body timeouts may narrow transport phases but never extend semantic budgets. Permit saturation therefore cannot hide outside reported latency.

Retries are disabled by default. An opt-in remote-source policy may retry only a query call explicitly considered replay-safe, only for configured transient transport/status failures, and only before accepting a valid GraphQL source result. Mutations and local Caliban calls are never retried implicitly. One logical call retains its source permit across the bounded attempt sequence, including jittered backoff, so overload cannot multiply source concurrency. Attempt count, elapsed retry time, source timeout, and request deadline all bound it; each failed attempt releases its response body before the next.

Core telemetry is low-allocation and non-blocking: capture admission/queue time, node readiness, permit wait, attempts, source duration/status/bytes, integration duration, cancellation, and final outcome. Source-call spans inherit request trace context through ZIO. Exporters consume bounded asynchronous events and may aggregate/drop under their own overload policy; they cannot backpressure execution. Intentionally effectful extension hooks remain only at coarse request/source boundaries, and their time counts against the applicable deadline.

The canonical execution fixture is a client query for remote Products with a local-Caliban Price field and a conditionally included remote Reviews field:

1. acquire bounded admission; establish the request scope, deadline, bound variables, graph-generation lease, response store, and coordinator arrays;
2. evaluate the condition directly and launch Products in a scoped fiber;
3. Products waits for its source permit, executes, ingests a valid result, transfers ownership to the completion queue, and releases the permit;
4. the coordinator integrates Products, gathers/deduplicates entity inputs, and releases local Pricing plus enabled remote Reviews concurrently under separate source permits;
5. if Reviews returns partial data/errors, integrate both; if Pricing defects, apply Caliban-style `ExecutionError` masking and null propagation while retaining unaffected product items;
6. skip empty/ineligible descendants, cancel newly unreachable work, and project the final result in client order;
7. if the caller instead interrupts, the scope interrupts permit waiters, retries, HTTP work, local Caliban, operation resolver/policy/header effects, and coordinator work and finalizes queued results/buffers without producing a response; any user-provided effect that remains uninterruptible stays inside the request scope and can delay completion rather than being detached.

The same coordinator executes mutations; planner fences alone delay the next top-level mutation root until the preceding routed subtree is complete.

During implementation, benchmark the compact coordinator against a simple recursive/`foreachPar` reference with identical semantics. Cover deep chains, wide ready sets, mixed delays, completion storms, source-permit contention, partial failures, and cancellation. Measure throughput, latency, allocation, fibers, queue operations, fairness, and cleanup latency. Plans, sources, and public engine behavior depend only on the coordinator contract, so internal queue/fiber mechanics may simplify if JVM measurements favor the reference implementation.
