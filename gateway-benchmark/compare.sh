#!/usr/bin/env bash
set -u

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <graphql-gateway-benchmarks-checkout> <gateway>..." >&2
    echo "Gateways are directory names under apollo-federation/gateways, or fusion for composite-schema/gateways/fusion." >&2
    exit 1
fi

UPSTREAM_DIR=$(CDPATH= cd -- "$1" && pwd)
shift
SUBGRAPHS=${SUBGRAPHS:-rust}
DELAY_MS=${DELAY_MS:-4}
WARMUP_SECONDS=${WARMUP_SECONDS:-30}
MEASURE_SECONDS=${MEASURE_SECONDS:-60}
COOL_SECONDS=${COOL_SECONDS:-30}
BENCH_VUS=${BENCH_VUS:-50}
RESULTS_DIR=${RESULTS_DIR:-$UPSTREAM_DIR/benchmark-runs/local-$(date +%Y%m%d-%H%M%S)}
PROBE='{"query":"{ __typename }"}'
GATEWAY_URL=http://127.0.0.1:5220/graphql
SUBGRAPHS_FAMILY=""

kill_listeners() {
    local pids
    pids=$(lsof -tiTCP:"$1" -sTCP:LISTEN 2>/dev/null || true)
    [ -n "$pids" ] && kill -9 $pids 2>/dev/null
    return 0
}

probe() {
    curl -fsS --max-time 2 -X POST -H 'content-type: application/json' -d "$PROBE" "$1" 2>/dev/null | grep -q '"data"'
}

stop_subgraphs() {
    for port in 5221 5222 5223 5224; do kill_listeners "$port"; done
    SUBGRAPHS_FAMILY=""
}

start_subgraphs() {
    local family=$1
    [ "$SUBGRAPHS_FAMILY" = "$family" ] && return 0
    stop_subgraphs
    local dir="$UPSTREAM_DIR/$family/subgraphs-$SUBGRAPHS"
    if [ "$SUBGRAPHS" = rust ]; then
        (cd "$dir" && SUBGRAPH_DELAY_MS=$DELAY_MS bash start.sh) &
    else
        (cd "$dir" && BENCHMARK_SIMULATE_LATENCY=$([ "$DELAY_MS" != 0 ] && echo 1) bash start.sh) &
    fi
    for _ in $(seq 1 240); do
        local up=0
        for port in 5221 5222 5223 5224; do probe "http://127.0.0.1:$port/graphql" && up=$((up + 1)); done
        if [ "$up" = 4 ]; then SUBGRAPHS_FAMILY=$family; return 0; fi
        sleep 0.5
    done
    echo "Subgraphs in $dir did not become ready." >&2
    exit 1
}

cleanup() {
    kill_listeners 5220
    stop_subgraphs
}
trap cleanup EXIT INT TERM

mkdir -p "$RESULTS_DIR"
for gateway in "$@"; do
    if [ "$gateway" = fusion ]; then
        family=composite-schema
    else
        family=apollo-federation
    fi
    gateway_dir="$UPSTREAM_DIR/$family/gateways/$gateway"
    [ -x "$gateway_dir/start.sh" ] || { echo "No start.sh in $gateway_dir." >&2; exit 1; }
    start_subgraphs "$family"
    echo "=== $gateway ($SUBGRAPHS subgraphs, ${DELAY_MS}ms delay) $(date +%H:%M:%S)"
    kill_listeners 5220
    (cd "$gateway_dir" && bash start.sh) &
    leader=$!
    ready=0
    for _ in $(seq 1 240); do
        if probe "$GATEWAY_URL"; then ready=1; break; fi
        sleep 0.5
    done
    if [ "$ready" != 1 ]; then
        echo "$gateway did not become ready; last log lines:" >&2
        tail -20 "$gateway_dir/gateway_log.txt" 2>/dev/null >&2
        pkill -P "$leader" 2>/dev/null; kill "$leader" 2>/dev/null; kill_listeners 5220
        continue
    fi
    out="$RESULTS_DIR/$gateway"
    mkdir -p "$out"
    k6 run --quiet -e MODE=constant -e BENCH_VUS="$BENCH_VUS" -e BENCH_OVER_TIME="${WARMUP_SECONDS}s" \
        -e GATEWAY_ENDPOINT="$GATEWAY_URL" "$UPSTREAM_DIR/k6/k6.js" > /dev/null 2>&1
    k6 run --quiet -e MODE=constant -e BENCH_VUS="$BENCH_VUS" -e BENCH_OVER_TIME="${MEASURE_SECONDS}s" \
        -e GATEWAY_ENDPOINT="$GATEWAY_URL" -e SUMMARY_PATH="$out" "$UPSTREAM_DIR/k6/k6.js" > "$out/k6_stdout.txt" 2>&1
    pkill -P "$leader" 2>/dev/null; kill "$leader" 2>/dev/null; sleep 1; kill_listeners 5220
    python3 - "$out/k6_summary.json" "$gateway" <<'PY'
import json, sys
metrics = json.load(open(sys.argv[1]))["metrics"]
duration = metrics["http_req_duration"]["values"]
checks = metrics.get("checks", {}).get("values", {})
print(f"{sys.argv[2]}: {metrics['http_reqs']['values']['rate']:.0f} req/s"
      f"  med={duration['med']:.1f}ms p95={duration['p(95)']:.1f}ms  check_fails={checks.get('fails', 0)}")
PY
    sleep "$COOL_SECONDS"
done
echo "Results: $RESULTS_DIR"
