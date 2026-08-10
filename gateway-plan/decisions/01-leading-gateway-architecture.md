# How do leading gateways divide composition, artifacts, planning, execution, and reload?

Type: `research`
Status: `resolved`

## Question

Across the pinned Apollo Router, Hive Router, Cosmo, and Hot Chocolate Fusion sources, what architectural boundaries and lifecycle patterns are shared or divergent for offline composition, execution artifacts, schema loading, query planning, execution, cache ownership, hot reload, and in-flight request retirement? Identify the majority choices, their stated or evident trade-offs, and any design that depends on implementation-language constraints rather than the GraphQL domain.

## Answer

All four gateways compose outside the request path, build a complete immutable graph generation before publication, plan operations lazily into generation-owned caches, retain the last valid generation on a failed reload, and drain finite in-flight work while explicitly retiring long-lived streams. Artifact format is a 2–2 split: Apollo and Hive consume Federation/JOIN SDL, while Cosmo and Fusion consume richer versioned execution packages. The Scala design should therefore accept standard SDL but deploy a versioned compiled artifact through one `GraphGeneration` builder, publish it atomically, lease one generation per request, and use ZIO scopes for precise drain and retirement. Full evidence, divergences, and recommendations: [Leading gateway architecture: composition, generations, planning, execution, and reload](../research/01-leading-gateway-architecture.md).
