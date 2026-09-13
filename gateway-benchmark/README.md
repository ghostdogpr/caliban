# Caliban GraphQL Gateway Benchmarks adapter

Runs Caliban's Quick HTTP path inside the Apollo Federation family of the
[ChilliCream GraphQL Gateway Benchmarks](https://github.com/ChilliCream/graphql-gateway-benchmarks). The pinned revision is in
[`upstream.env`](upstream.env); `prepare-upstream.sh` refuses any other checkout.

The adapter acquires the four Federation schemas from the subgraphs, builds a `GatewayInterpreter`, serves it with `QuickAdapter`
on port 5220, and forwards the `Authorization` header to subgraph calls like the other tracked gateways.

## Prepare

Install k6, jq, Python 3, curl, sbt, a JDK, and the Rust toolchain.

```sh
git clone https://github.com/ChilliCream/graphql-gateway-benchmarks.git /path/to/graphql-gateway-benchmarks
. ./gateway-benchmark/upstream.env
git -C /path/to/graphql-gateway-benchmarks checkout "$GRAPHQL_GATEWAY_BENCHMARKS_REVISION"
./gateway-benchmark/prepare-upstream.sh /path/to/graphql-gateway-benchmarks
```

## Run with the upstream driver

The driver needs Bash 4 or newer (on macOS, install it with Homebrew).

```sh
cd /path/to/graphql-gateway-benchmarks
SUBGRAPH_DELAY_MS=4 ./k6/benchmark.sh apollo-federation/gateways/caliban subgraphs-rust constant-latency
```

Set `SUBGRAPH_DELAY_MS=4` for `constant-latency` with the Rust subgraphs: the driver only exports `BENCHMARK_SIMULATE_LATENCY=1`,
which the Rust subgraphs ignore. `MEASURE_SECONDS=60 BENCH_RUNS=3` gives a quick pass instead of nine 120-second runs.

Run competitors with the same arguments and one of these gateway paths: `apollo-federation/gateways/apollo-router`,
`apollo-federation/gateways/hive-router`, `apollo-federation/gateways/cosmo`, `composite-schema/gateways/fusion`.

## Run a local comparison

`compare.sh` runs one constant-load pass over several gateways with the upstream `k6.js` and start scripts. It only needs the
system Bash. Install each gateway with its upstream `install.sh` and build the subgraphs with their `build.sh` first.

```sh
./gateway-benchmark/compare.sh /path/to/graphql-gateway-benchmarks hive-router cosmo caliban fusion apollo-router
```

Per gateway: start, warm up 30 s, measure 60 s at 50 VUs, stop, cool down 30 s, against the Rust subgraphs with a 4 ms delay.
`SUBGRAPHS=net`, `DELAY_MS`, `WARMUP_SECONDS`, `MEASURE_SECONDS`, `COOL_SECONDS`, and `BENCH_VUS` override those. Summaries are
written under `benchmark-runs/local-<timestamp>` in the upstream checkout.

## Adapter settings

| Variable | Default | Effect |
| --- | --- | --- |
| `JAVA` | `java` | JVM executable |
| `JAVA_OPTS` | none | Extra JVM flags |
| `BENCHMARK_GATEWAY_PORT` | `5220` | Listening port |
| `BENCHMARK_SUBGRAPHS_HOST` | `127.0.0.1` | Host of the subgraphs on ports 5221 to 5224 |

## Profiles

```sh
JAVA_OPTS='-Xlog:gc*:file=/tmp/caliban-gateway-gc.log:time,uptime,level,tags -XX:NativeMemoryTracking=summary' \
  ./gateway-benchmark/run.sh
asprof -d 30 -e cpu -f /tmp/caliban-gateway-cpu.html PID
asprof -d 30 -e alloc -f /tmp/caliban-gateway-alloc.html PID
jcmd PID GC.heap_info
```

Profiles are not checked in.
