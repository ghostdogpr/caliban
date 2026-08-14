# Response-assembly measurement spike

> Historical experiment: its measurements remain useful, but its proposed production types and sequencing are not requirements. The current [implementation handoff](../../IMPLEMENTATION-HANDOFF.md) is authoritative.

This throwaway JMH spike compares three complete source-response paths for the
same federated entity join:

1. decode both source responses into Caliban `ResponseValue`, use object lookup
   and `deepMerge`, project a new tree, then encode with jsoniter;
2. run a plan-specialized jsoniter reader into typed indexed arrays, retain
   final-only JSON subtrees as copied raw values, and write the response
   directly;
3. scan source bytes into packed primitive token spans, retain final-only JSON
   as slices of the owned source buffers, and copy those spans through a direct
   final writer.

Each product joins four reviews. The error-heavy case makes every third
non-null `price` resolve to null, propagates that null to the nullable product
item, and preserves the corresponding GraphQL errors. Trial setup parses all
three outputs back to `ResponseValue` and requires semantic equality before
JMH can run.

The specialized readers model compiled source-result programs: field names,
types, merge destinations, and the final projection are already known from the
planned operation. They are deliberately not general JSON DOM parsers.

## Reproduce

From the repository root:

```bash
sbt '++3.3.8' \
  'set benchmarks / Compile / unmanagedSourceDirectories += file("gateway-plan/prototypes/10-response-assembly")' \
  'benchmarks/Jmh/run -prof gc -wi 3 -i 5 -w 1s -r 1s -f 1 .*GatewayResponseAssemblyBenchmark.*'
```

The larger directional run used `-p productCount=512 -wi 2 -i 3`.

## Results

Environment: JMH 1.37, Scala 3.3.8, OpenJDK 25.0.1, one benchmark thread and
one fork. These are local architectural measurements, not competitive router
numbers.

### 128 products, five measured iterations

| Workload | Representation | Throughput | Allocated bytes/op |
|---|---|---:|---:|
| join-heavy | `ResponseValue` | 4,347 ops/s | 977,250 B |
| join-heavy | indexed materialized/raw-copy | 22,821 ops/s | 272,729 B |
| join-heavy | packed raw index/slice | 18,380 ops/s | 172,272 B |
| error-heavy | `ResponseValue` | 5,312 ops/s | 908,961 B |
| error-heavy | indexed materialized/raw-copy | 22,073 ops/s | 259,177 B |
| error-heavy | packed raw index/slice | 18,114 ops/s | 158,696 B |

The indexed jsoniter path was 5.25x/4.16x the `ResponseValue` throughput and
allocated 72%/71% less on the join/error workloads. The raw-slice path traded
about 18–19% of that specialized throughput for another 37–39% allocation
reduction.

### 512 products, three measured iterations

| Workload | Representation | Throughput | Allocated bytes/op |
|---|---|---:|---:|
| join-heavy | `ResponseValue` | 1,081 ops/s | 4,001,839 B |
| join-heavy | indexed materialized/raw-copy | 5,222 ops/s | 1,192,651 B |
| join-heavy | packed raw index/slice | 4,766 ops/s | 691,569 B |
| error-heavy | `ResponseValue` | 1,321 ops/s | 3,744,867 B |
| error-heavy | indexed materialized/raw-copy | 5,608 ops/s | 1,121,451 B |
| error-heavy | packed raw index/slice | 4,658 ops/s | 636,810 B |

The short larger run is directional: its throughput confidence intervals are
wide. It nevertheless reproduces the representation ordering and allocation
scaling. The raw index is within 9% of the indexed path for the large join while
allocating 42% less; the error-heavy case is 17% slower and allocates 43% less.

## Interpretation and limits

`ResponseValue` is a useful Caliban interoperability value but is not viable as
the gateway assembly hot path. The result favors a hybrid indexed store:
materialize routing/nullability values into primitive slots, retain untouched
output values as raw references, and project directly in client order.

The hand-written raw scanner intentionally uses ordinary string keys and is
not a production parser. A compiled UTF-8 field matcher could recover some of
its throughput, but this spike does not prove that it will. Conversely,
jsoniter's public `readRawValAsBytes` copies every retained raw value, so the
fast indexed prototype overstates the allocation cost of a store that can
reference an owned source buffer. The selected design combines the measured
strengths instead of adopting either prototype literally.

This fixture covers entity fan-out shape, direct projection, a non-null field
propagating to a nullable list item, and an error-heavy envelope. Alias-aware
subgraph error-path rewriting, abstract types, duplicate-entity fan-out, local
Caliban import, and incremental payload lifetime remain mandatory conformance
and pipeline tests; their algorithms are not independently microbenchmarked
here.
