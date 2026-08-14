# Operation-front-end measurement spike

> Historical experiment: its measurements remain useful, but its proposed production types and sequencing are not requirements. The current [implementation handoff](../../IMPLEMENTATION-HANDOFF.md) is authoritative.

This throwaway Scala 3/JMH spike answers two questions needed by the gateway architecture:

1. Is Caliban's existing parser an adequate default, or is there already evidence to build a specialized parser?
2. Which parsing/validation work must a warm gateway operation cache eliminate?

The executable benchmark is [GatewayOperationFrontEndBenchmark.scala](GatewayOperationFrontEndBenchmark.scala). It uses a valid code-first Caliban schema, a named operation with a fragment and directive, three variables, and a recursive input object 20 levels deep. The benchmark was compiled against the working tree's Caliban core and run on OpenJDK 25.0.1 with Scala 3.3.8.

## Results

These are short calibration runs: one fork, one thread, three one-second warmups, three one-second measurements, and JMH's GC profiler. They are sufficient for choosing the seam, not release-grade performance claims.

| Path | Throughput | Allocated per operation | Meaning |
| --- | ---: | ---: | --- |
| Cold Caliban pipeline | 114,008 ops/s | 31,744 B | Parse, coerce variables, validate, materialize fields |
| Parsed-document cache hit | 169,215 ops/s | 23,904 B | Skip parsing, but repeat validation and materialization |
| Statically validated document hit | 540,686 ops/s | 8,272 B | Skip parsing and static validation; coerce variables and materialize fields |
| Prepared-request lookup lower bound | 573.1 M ops/s | approximately 0 B | Concurrent map lookup only; not a valid cross-request representation |
| Parse only | 427,890 ops/s | 7,816 B | Caliban `Parser` on the representative operation |
| Static validation only | 326,956 ops/s | 15,792 B | Caliban `Validator.validateAll` |
| Variable coercion only | 716,564 ops/s | 4,600 B | Recursive input value validation/coercion |
| Field creation only | 1.93 M ops/s | 3,752 B | Caliban `Validator.prepare` with static validation skipped |

The statically validated hit is about 4.7 times the cold throughput and allocates 74% less. A parsed-AST-only cache is only about 1.5 times the cold throughput and saves 25% of allocation. Static validation is therefore too expensive to repeat on the warm path.

The lookup-only result is deliberately just a lower bound. Caliban's `ExecutionRequest`/`Field` tree contains concrete variable values, so caching it would return another request's variables. It demonstrates the available headroom but cannot be the gateway cache value.

An existing parser comparison was also rerun under the same Scala/JDK with the full introspection operation:

| Parser | Throughput | Allocated per operation |
| --- | ---: | ---: |
| Caliban FastParse parser | 145,130 ops/s | 34,256 B |
| `gql` cats-parse parser | 18,169 ops/s | 278,335 B |

On that large operation Caliban is about eight times faster and allocates about one eighth as much as the available Scala alternative. This does not prove that a purpose-built UTF-8 parser could never win, but it gives no reason to build one before cache-miss profiling shows parsing violates the gateway performance gate.

## Architectural conclusion

- Reuse Caliban's parser and executable-document AST for the initial implementation.
- Keep a bounded, schema-independent parsed-document cache because parsing is pure with respect to the graph generation.
- Produce a variable-independent `PreparedOperation`, not a Caliban `ExecutionRequest`. It contains the selected and statically validated operation, compact normalized selections, variable/argument programs, planning inputs, and precomputed limits. The generation-owned warm cache stores the later `PlannedOperation`, which adds its plan and response projection.
- Bind and coerce request variables after the planned-operation lookup into request-owned slots. Variables, headers, and ordinary request extensions are never embedded in the cached value.
- Coalesce preparation misses with keyed single-flight. Hits do not allocate miss-coordination objects or acquire the miss lock.
- Retain the parser, validator, normalizer, and cache behind one private operation-front-end seam so a specialized parser or compiled coercer can replace a stage without affecting the public gateway API.

## Reproduction

```text
sbt '++3.3.8' \
  'set benchmarks / Compile / unmanagedSourceDirectories += file("gateway-plan/prototypes/07-operation-front-end")' \
  'benchmarks/Jmh/run -wi 3 -i 3 -f 1 -t 1 -prof gc "caliban.gateway.prototype.GatewayOperationFrontEndBenchmark.*"'
```

The parser comparison used:

```text
sbt '++3.3.8' \
  'benchmarks/Jmh/run -wi 3 -i 3 -f 1 -t 1 -prof gc "caliban.ParserBenchmark.run(Caliban|Gql)"'
```
