# Caliban GraphQL Gateways Benchmark adapter

This non-published project runs Caliban's production Quick HTTP path against the pinned
[GraphQL Gateways Benchmark](https://github.com/graphql-hive/graphql-gateways-benchmark). The revision is recorded once in
[`upstream.env`](upstream.env); `prepare-upstream.sh` refuses any other checkout.

The adapter acquires the four authored Federation schemas from the benchmark subgraphs, builds a normal `GatewayInterpreter`, and
serves it with `QuickAdapter`. It does not consume the benchmark's serialized supergraph. It explicitly enables Caliban's
default-disabled in-flight query deduplication for each remote source: overlapping identical queries share one running downstream
call. The tested competitor versions use the equivalent behavior. A unique-header control is available so the benefit can be
separated from general execution speed.

## Prepare and run

```sh
git clone https://github.com/graphql-hive/graphql-gateways-benchmark.git /path/to/graphql-gateways-benchmark
. ./gateway-benchmark/upstream.env
git -C /path/to/graphql-gateways-benchmark checkout "$GRAPHQL_GATEWAYS_BENCHMARK_REVISION"
./gateway-benchmark/prepare-upstream.sh /path/to/graphql-gateways-benchmark
./gateway-benchmark/install.sh
```

Start the upstream subgraphs, then run the benchmark through the upstream driver:

```sh
make -C /path/to/graphql-gateways-benchmark run-subgraphs
WAIT_FOR_URL=http://127.0.0.1:4000/health \
  make -C /path/to/graphql-gateways-benchmark test gateway=caliban mode=constant

WAIT_FOR_URL=http://127.0.0.1:4000/health \
  make -C /path/to/graphql-gateways-benchmark test gateway=caliban mode=stress
```

The pinned driver's constant profile uses a 15-second warmup followed by 50 virtual users for 60 measured seconds. Its stress profile
uses the same warmup, then ramps from 0 to 50, 500, and back to 50 users over 60 seconds. Use the same unmodified profile for each
comparator. The recorded runs used k6 0.54.0.

`BENCHMARK_SUBGRAPHS_URL` changes the source base URL and `BENCHMARK_GATEWAY_PORT` changes the adapter port. The defaults are
`http://127.0.0.1:4200` and `4000`. `BENCHMARK_UNIQUE_SOURCE_HEADERS=true` gives every logical source call a unique harmless header;
it exists only for the non-deduplicable control and must not be set for the primary comparison.

## Measurement contract

The pinned benchmark is run without modifying its driver or tracked gateway configurations. `prepare-upstream.sh` rejects a checkout
with tracked changes, then adds only an untracked Caliban adapter directory containing `run.sh` and `target` symlinks. Results use the
upstream driver's operation, response checks, setup, warmup, load profile, metrics, and router defaults exactly as published at the
pinned revision.

The comparison reports the upstream `iterations` rate, which counts measured scenario iterations rather than requests made by the
driver's setup function. The upstream checks still require a `200` response, no GraphQL errors, and the expected response structure.

## Comparable configurations

All measured implementations use the same four upstream endpoints, operation text, response checker, warmup, measured duration,
and virtual-user count. The native routers retain the pinned upstream defaults. Apollo's pinned adapter explicitly enables query
deduplication, while the tested Cosmo, Grafbase, and Hive versions enable equivalent outbound sharing by default. Caliban's declared
adapter override is therefore comparable, but the unique-header control remains necessary because the repeated-query workload is
an unusually favorable case.

| Implementation | Artifact at the published revision | Input | Telemetry |
| --- | --- | --- | --- |
| Caliban | This directory | Authored SDL acquired from `_service` | None installed |
| Apollo Router 2.6.0 | Upstream `apollo-router` | Upstream supergraph | Upstream run script |
| Hive Router 0.0.8 installer | Upstream `hive-router` | Upstream supergraph | Upstream run script |
| Grafbase Gateway 0.49.0 | Upstream `grafbase` | Upstream federated graph | Upstream run script |
| Cosmo Router 0.247.0 | Upstream `cosmo` | Upstream engine config | Upstream run script |
| Hot Chocolate Fusion | Not present upstream | Fusion archive required | Not available |

The dashboard labels Hive Router 0.0.5, while the installer checked into the corresponding source revision requests 0.0.8. Both old
Hive release assets are now unavailable; the same-host comparison therefore uses 0.0.43 with the published configuration. The pinned
repository also has no Hot Chocolate Fusion adapter. These provenance gaps prevent a defensible release ranking from this seed
workload alone.

## Profiles

Use the upstream monitor for process CPU and resident memory. For JVM profiles, launch Caliban with GC and native-memory tracking:

```sh
JAVA_OPTS='-Xlog:gc*:file=/tmp/caliban-gateway-gc.log:time,uptime,level,tags -XX:NativeMemoryTracking=summary' \
  ./gateway-benchmark/run.sh
```

During the measured interval, capture CPU and allocation flamegraphs with async-profiler and take memory snapshots with `jcmd`:

```sh
asprof -d 30 -e cpu -f /tmp/caliban-gateway-cpu.html PID
asprof -d 30 -e alloc -f /tmp/caliban-gateway-alloc.html PID
jcmd PID GC.heap_info
jcmd PID VM.native_memory summary
```

Profiles are deliberately not checked in: they contain machine-specific symbols and are easy to mistake for comparable results.
