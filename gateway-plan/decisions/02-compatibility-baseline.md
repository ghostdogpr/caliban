# Which GraphQL and Federation semantics define the compatibility baseline?

Type: `research`
Status: `resolved`

## Question

Using the applicable GraphQL, GraphQL-over-HTTP, Federation 2, and JOIN specifications plus the pinned gateway sources and their compatibility suites, which externally observable semantics and Federation features must the first production-capable Caliban Graph Router support? Where specifications leave latitude, establish the majority behavior and document meaningful incompatibilities among the four gateways.

## Answer

Adopt GraphQL September 2025 and the GraphQL-over-HTTP JSON profiles; make core Federation 2 composition and execution through entity interfaces the unary v1 gate; use released JOIN v0.3 as the portable artifact boundary; and stage subscriptions, `@defer`, contexts, progressive overrides, and `@stream` behind explicit later milestones. Where implementations diverge, preserve spec-correct response completion and HTTP negotiation, safe alias-aware subgraph error propagation, deterministic but implementation-independent plans, and fail-closed handling of composed security metadata. See [GraphQL and Federation compatibility baseline](../research/02-compatibility-baseline.md).
