#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <graphql-gateways-benchmark-checkout>" >&2
    exit 1
fi

SCRIPT_DIR=$(CDPATH= cd -P -- "$(dirname -- "$0")" && pwd)
UPSTREAM_DIR=$(CDPATH= cd -- "$1" && pwd)
. "$SCRIPT_DIR/upstream.env"
EXPECTED_REVISION=$GRAPHQL_GATEWAYS_BENCHMARK_REVISION
ACTUAL_REVISION=$(git -C "$UPSTREAM_DIR" rev-parse HEAD)

if [ -z "$EXPECTED_REVISION" ] || [ "$ACTUAL_REVISION" != "$EXPECTED_REVISION" ]; then
    echo "Expected GraphQL Gateways Benchmark revision $EXPECTED_REVISION, found $ACTUAL_REVISION." >&2
    exit 1
fi

if ! git -C "$UPSTREAM_DIR" diff --quiet || ! git -C "$UPSTREAM_DIR" diff --cached --quiet; then
    echo "The pinned benchmark checkout has tracked changes; restore it before running the benchmark." >&2
    exit 1
fi

ADAPTER_DIR="$UPSTREAM_DIR/gateways/caliban"
if [ -L "$ADAPTER_DIR" ]; then
    rm "$ADAPTER_DIR"
fi
mkdir -p "$ADAPTER_DIR"
ln -sfn "$SCRIPT_DIR/run.sh" "$ADAPTER_DIR/run.sh"
ln -sfn "$SCRIPT_DIR/target" "$ADAPTER_DIR/target"

echo "Prepared pinned GraphQL Gateways Benchmark checkout at $UPSTREAM_DIR."
