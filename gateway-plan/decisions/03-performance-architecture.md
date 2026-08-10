# Which architecture choices dominate gateway hot-path performance?

Type: `research`
Status: `resolved`

## Question

Across the pinned gateway implementations and their benchmarks or profiling guidance, which representations, algorithms, cache designs, concurrency choices, JSON techniques, networking choices, and allocation controls dominate parsing, validation, planning, subgraph execution, and response assembly performance? Compare those findings with reusable Caliban components in the current repository and identify the seams where reuse needs measurement or a specialized replacement to meet the agreed performance gate.

## Answer

[Performance architecture of the Caliban graph router](../research/03-performance-architecture.md) finds a strong majority architecture: immutable schema-generation state; weighted, schema-scoped and single-flight operation/plan caches; primitive-indexed dependency plans; bounded structured concurrency; pooled transports; and a byte-oriented, raw-value-capable response store. Reuse Caliban for GraphQL parsing/validation semantics, schema boundaries, jsoniter baselines, ZIO, Federation subgraph support, and prepared local execution, but keep parser, transport, scheduler, and especially response assembly behind benchmark gates; do not make the general `ResponseValue`/interpreter pipeline the assumed remote-routing hot path.
